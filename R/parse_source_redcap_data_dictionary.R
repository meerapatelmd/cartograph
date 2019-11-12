#' Parses source version of REDCAP data dictionary into variables and permissible values
#' @param input.file source REDCap data dictionary as csv
#' @param write_excel_file TRUE if an excel output with the same basename with "_parsed" appended is desired.
#' @return list of dataframes for each Variables, Permissible Values, and Data Type Map
#' @import typewriteR
#' @import stringr
#' @import openxlsx
#' @export

parse_source_redcap_data_dictionary <-
        function(input.file, write_excel_file = TRUE) {
                require(stringr)
                require(zip)       ## we need to load zip manualy as of the writing of this script
                require(openxlsx)

                ##Reading csv
                df.raw <- read.csv(input.file, header = TRUE, stringsAsFactors = FALSE)

                ## Pattern selection translates as remove anything after and including the last period
                project.name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(input.file))

                ## These empty vectors must be defined before the for loop ##
                pv.literal <- c()
                pv.names <- c()

                ## This for loop cycles through all of the radio dropdown and checkbox variables                                 ##
                ## It splits the pipe delineated values and places them into the pv.literal vector and removes the whitespaces  ##
                ## When it splits a value it fills the pv.names vector with the names of the variables for each extracted value  ##
                for (i in 1:length(df.raw$Choices..Calculations..OR.Slider.Labels)){

                        if(df.raw$Field.Type[i] %in% c('radio', 'dropdown', 'checkbox')) {

                                ##Making sure numbers are present for all options
                                pv.literal_of_i <- str_trim(unlist(strsplit(df.raw$Choices..Calculations..OR.Slider.Labels[i], "|", fixed = TRUE)), "both")
                                pv.literal_of_i <- str_replace_all(pv.literal_of_i, "([a-zA-Z]{1,})[,]{1}( {0,}NOS)", "\\1\\2")
                                pv.literal_of_i <- str_replace_all(pv.literal_of_i, "(NED)[,]{1}( )", "\\1\\2")
                                pv.literal_of_i <- str_replace_all(pv.literal_of_i, "(^[0-9]{1,3})( )", "\\1,\\2")

                                if (length(pv.literal_of_i) > 1) {
                                        first.pv.literal_of_i <- pv.literal_of_i[1]
                                        last.pv.literal_of_i <- pv.literal_of_i[length(pv.literal_of_i)]
                                        other.pv.literal_of_i <- pv.literal_of_i[!(pv.literal_of_i %in% c(first.pv.literal_of_i, last.pv.literal_of_i))]

                                        if (grepl("[0-9][,] ", first.pv.literal_of_i) == FALSE) {
                                                first.pv.literal_of_i <- paste0("1, ", first.pv.literal_of_i)
                                        }

                                        if (grepl("[0-9][,] ", last.pv.literal_of_i) == FALSE) {
                                                new_number <- as.character(1 + max(as.integer(str_remove_all(pv.literal_of_i, "[^0-9]")), na.rm = TRUE))
                                                last.pv.literal_of_i <- paste0(new_number, ", ", last.pv.literal_of_i)
                                        }

                                        final.pv.literal_of_i <- c(first.pv.literal_of_i, other.pv.literal_of_i, last.pv.literal_of_i)
                                } else if (length(pv.literal_of_i) == 1) {
                                                final.pv.literal_of_i <- pv.literal_of_i
                                                print(final.pv.literal_of_i)
                                                typewriteR::tell_me("This is a radio/dropdown/checkbox field type, but there is only 1 permissible value. Investigate.")
                                                typewriteR::stop_and_enter()
                                }


                                if (length(pv.literal_of_i) != length(final.pv.literal_of_i)) {
                                        typewriteR::tell_me("pv.literal_of_i:", pv.literal_of_i)
                                        typewriteR::tell_me("final.pv.literal_of_i:", final.pv.literal_of_i)
                                        typewriteR::stop_and_enter()
                                }

                                pv.literal <-  c(pv.literal, final.pv.literal_of_i)


                                pv.names <- c(pv.names, rep_len(
                                        df.raw$Variable...Field.Name[i],
                                        length.out = length(final.pv.literal_of_i)))

                        }

                        if (df.raw$Field.Type[i] %in% c('yesno')) {
                                final.pv.literal_of_i <- c("1, yes", "2, no")

                                pv.literal <-  c(pv.literal, final.pv.literal_of_i)

                                pv.names <- c(pv.names, rep_len(
                                        df.raw$Variable...Field.Name[i],
                                        length.out = length(final.pv.literal_of_i)))
                        }
                }

                ## Variables coloumns ##
                v.project <- rep_len(project.name, length.out = length(df.raw$Variable...Field.Name))
                v.phi <- rep_len("", length.out = length(df.raw$Variable...Field.Name))
                v.id <- rep_len("", length.out = length(df.raw$Variable...Field.Name))


                ## Permissiable_Values coloums ##
                pv.project <- rep_len(project.name, length.out = length(pv.literal))
                pv.con.id <- rep_len("", length.out = length(pv.literal))
                pv.names <- str_trim(pv.names)
                pv.literal <- str_trim(pv.literal)

                pv.value.code <- str_trim(sub("[,]{1}.*$", "", pv.literal)) ## Patern matching ",.*$" translates as remove all characters after comma
                pv.label <- str_trim(sub(".*?[,]{1}", "", pv.literal))       ## Patern matching ".*," translates as remove all characters before comma

                ##Checkpoint to ensure correct parsing
                checkpoint_01 <-
                        data.frame(
                                   pv.project = pv.project,
                                   pv.con.id = pv.con.id,
                                   pv.names  = pv.names,
                                   pv.literal = pv.literal,
                                   pv.value.code = pv.value.code,
                                   pv.label = pv.label) %>%
                        dplyr::mutate(checkpoint_01 = as.integer(pv.value.code)) %>%
                        dplyr::filter(is.na(checkpoint_01))

                if (nrow(checkpoint_01) > 0) {
                        print(checkpoint_01)
                        typewriteR::tell_me("ERROR: parsing failed. See above.")
                        typewriteR::stop_and_enter()
                }

                ## Data type mapping coloumns ##
                dtm.variable.type <- c("checkbox", "date_mdy", "dropdown", "integer", "notes", "number_2dp", "radio", "text", "yesno", "calc")
                dtm.url <- c("http://mskcc.org/ontologies/metadata#MSKO0000224",
                             "http://mskcc.org/ontologies/metadata#MSKO0000223",
                             "http://mskcc.org/ontologies/metadata#MSKO0000224",
                             "http://mskcc.org/ontologies/metadata#MSKO0000221",
                             "http://mskcc.org/ontologies/metadata#MSKO0000238",
                             "http://mskcc.org/ontologies/metadata#MSKO0000222",
                             "http://mskcc.org/ontologies/metadata#MSKO0000224",
                             "http://mskcc.org/ontologies/metadata#MSKO0000238",
                             "http://mskcc.org/ontologies/metadata#MSKO0000324",
                             "http://mskcc.org/ontologies/metadata#MSKO0000342")
                dtm.data.type <- c("value set", "date", "value set", "integer", "string", "float", "value set", "string", "boolean", "custom data type")


                ## All of our relevent vectors are formed into dataframes ##
                Variables <- data.frame(v.project, df.raw$Form.Name, df.raw$Variable...Field.Name, df.raw$Field.Label, df.raw$Field.Type, v.phi, v.id)
                colnames(Variables) <- c("Project Name", "Form Name", "Variable Name", "Variable Label", "Variable Type", "PHI", "MSK Concept ID")

                Permissible_Values <- data.frame(pv.project, pv.names, pv.literal, as.numeric(pv.value.code), pv.label, pv.con.id)
                colnames(Permissible_Values) <- c("Project Name", "Variable Name", "Permissible Value Literal", "Permissible Value Code", "Permissible Value Label", "MSK Concept ID")

                Data_Type_Mappings <- data.frame(dtm.variable.type, dtm.url, dtm.data.type)
                colnames(Data_Type_Mappings) <- c("Variable Type", "edg:physicalDatatype", "")

                Source_Version <- df.raw
                ## Dataframes need to be compiled into a list to have each printed as a workbook ##
                final.book <- list(Variables, Permissible_Values, Data_Type_Mappings, Source_Version)
                names(final.book) <- c("Variables", "Permissible_Values", "Data Type Mappings", "Source_Version")

                ## Creating new xlsx filename
                if (write_excel_file == TRUE) {
                        xlsx_fn <- stringr::str_replace_all(basename(input.file), "(.*)([.]{1}csv$)", "\\1_parsed.xlsx")
                        openxlsx::write.xlsx(final.book,
                                                      xlsx_fn,
                                                      sheetName = c("Variables", "Permissible_Values", "Data Type Mappings", "Source_Version"),
                                                      colWidths = "auto")

                        typewriteR::tell_me("\t", xlsx_fn, "successfully written.")

                        return(final.book)
                } else {
                        return(final.book)
                }
        }
