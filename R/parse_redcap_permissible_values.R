#' Parses REDCap Permissible Values
#' @import dplyr
#' @import stringr
#' @import centipede
#' @export

parse_redcap_permissible_values <-
        function(dataframe, id_col, variable_col, permissible_value_string_col, new_permissible_col_name = "PERMISSIBLE_VALUE_LITERAL") {
                permissible_value_string_col <- enquo(permissible_value_string_col)
                new_permissible_col_name <- enquo(new_permissible_col_name)
                variable_col <- enquo(variable_col)
                id_col <- enquo(id_col)

                pv.id        <- vector()
                pv.literal <- vector()
                pv.names <- vector()

                #Removing all FIELD_TYPE = "calc" because it parses the calculation formula into a PERMISSIBLE_VALUE
                dataframe <-
                        dataframe %>%
                        dplyr::filter(FIELD_TYPE != "calc")

                ##Making sure numbers are present for all options
                for (i in 1:nrow(dataframe)) {
                        id       <- dataframe %>%
                                dplyr::select(!!id_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()

                        variable <- dataframe %>%
                                dplyr::select(!!variable_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()

                        permissible_value_string <- dataframe %>%
                                dplyr::select(!!permissible_value_string_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()

                        if (!(is.na(permissible_value_string))) {
                                pv.literal_of_i <- centipede::strsplit_trim_pipe(permissible_value_string)
                                pv.literal_of_i <- stringr::str_replace_all(pv.literal_of_i, "(^[0-9]{1,3})( )", "\\1,\\2")
                                pv.literal_of_i <- stringr::str_replace_all(pv.literal_of_i, "(^[0-9]{1,4}[,]{1})(.*)([,]{1})(.*$)", "\\1\\2\\4")

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
                                }

                                pv.id <- c(pv.id, rep_len(id, length.out = length(final.pv.literal_of_i)))
                                pv.literal <-  c(pv.literal, final.pv.literal_of_i)
                                pv.names <- c(pv.names, rep_len(variable,
                                                                length.out = length(final.pv.literal_of_i)))
                        }

                }

                x <- data.frame(pv.id = pv.id,
                                pv.literal = pv.literal,
                                pv.names = pv.names
                )

                x <-
                        x %>%
                        dplyr::rename(!!id_col := pv.id,
                                      !!new_permissible_col_name := pv.literal,
                                      !!variable_col := pv.names)

                return(x)
        }
