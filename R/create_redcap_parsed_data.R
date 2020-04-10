#' Parses source redcap data dictionary to IDENTITY format
#' @param redcap_source_file path to csv export of REDCap data dictionary
#' @return identity format of the source redcap data dictionary, where there are 3 additional columns for the parsed permissible values, an ID column, and a timestamp
#' @import mirCat
#' @import rubix
#' @import dplyr
#' @import stringr
#' @export

create_redcap_parsed_data <-
        function(redcap_source_file) {
            
                    data <- broca::simply_read_csv(path_to_csv = redcap_source_file,
                                           log_details = paste0("read", redcap_source_file, " to parse"))

                    #Standardizing Column Names
                    data <- rubix::cleanup_colnames(data)

                #Adding IDENTITY_ID primary key
                DATA_02 <- data %>%
                        rubix::mutate_primary_key("REDCAP_GROUP_ID",
                                                  starting_number = 1,
                                                  width_left_pad_with_zero = 1+nchar(as.character(nrow(data))))

                #Saving a version of the processed data dictionary
                DATA_03_A <- DATA_02

                #Parsing the permissible values
                DATA_03_B <- parse_redcap_permissible_values(DATA_02,
                                                      id_col = REDCAP_GROUP_ID,
                                                      variable_col = VARIABLE_FIELD_NAME,
                                                      permissible_value_string_col = CHOICES_CALCULATIONS_OR_SLIDER_LABELS) %>%
                            dplyr::left_join(DATA_02)

                #Parsing boolean values
                DATA_03_C  <- parse_redcap_boolean_values(DATA_02,
                                                   id_col = REDCAP_GROUP_ID,
                                                   variable_col = VARIABLE_FIELD_NAME,
                                                   field_type_col = FIELD_TYPE) %>%
                                dplyr::left_join(DATA_02)

                #Combining all the parsed data into a single dataframe
                DATA_03 <- dplyr::bind_rows(DATA_03_A,
                                            DATA_03_B,
                                            DATA_03_C) %>%
                                        dplyr::distinct()

                #Performing final cleanup
                DATA_04 <-
                        DATA_03 %>%
                        dplyr::mutate(PERMISSIBLE_VALUE_CODE = stringr::str_remove_all(PERMISSIBLE_VALUE_LITERAL, "[,]{1}.*")) %>%
                        dplyr::mutate(PERMISSIBLE_VALUE_LABEL =  stringr::str_remove_all(PERMISSIBLE_VALUE_LITERAL, ".*?[,]{1}[ ]{0,1}"))

                #Adding REDCAP_CONCEPT_ID after deduplicating
                DATA_05 <-
                        DATA_04 %>%
                        dplyr::distinct() %>%
                        rubix::mutate_primary_key(pkey_column_name = "REDCAP_CONCEPT_ID",
                                                  starting_number = 1,
                                                  width_left_pad_with_zero = 1+nchar(as.character(nrow(DATA_04))))

                ##Arranging by REDCAP_GROUP_ID
                DATA_06 <-
                DATA_05 %>%
                        dplyr::arrange(REDCAP_GROUP_ID, REDCAP_CONCEPT_ID)

           ##Adding timestamp for REDCAP_GROUP_ID creation
                DATA_07 <- DATA_06

                ##Adding KEY variables
                DATA_08 <-
                        DATA_07 %>%
                        dplyr::mutate(TYPE = ifelse(is.na(PERMISSIBLE_VALUE_LABEL),
                                                         "Variable",
                                                         "Permissible Value")) %>%
                        dplyr::mutate(CONCEPT = ifelse(is.na(PERMISSIBLE_VALUE_LABEL),
                                                                VARIABLE_FIELD_NAME,
                                                                PERMISSIBLE_VALUE_LABEL))

                #Final
                DATA_XX_ <- DATA_08
                return(DATA_XX_)
        }
