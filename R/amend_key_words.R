#' Interactively collect keywords associated with a concept that will be used in the WHERE clause in the SQL statement
#' @param concept_col column to use to filter for the row to amend in the dataframe captured by "script_step_number, "_", output_dataframe_name, ".RData""
#' @param concept_to_amend character vector of length one of the exact concept in concept_col that requires amending
#' @param key_words_to_add character vector of length one separated by commas of new key_words to add to the string of keywords
#' @param output_dataframe_name character vector of length 1 that will be the name of the output data, which will be list split based on concept and additional column of the new terms
#' @param script_step_number the number assigned to the script this function is being run on. This is important because the R object will be saved with the script number as a prefix. If it is NULL, then it will not be saved.
#' @return dataframe with key_words with the addition of a new key_word
#' @import mirCat
#' @import crayon
#' @importFrom readr read_rds
#' @import dplyr
#' @import typewriteR
#' @export


amend_key_words <-
        function(path_to_key, identity_id, key_field, key_concept_name, ..., path_to_local_repo = NULL) {

                ###Arguments turn into a string of keywords
                Args <- list(...)
                new_key_words <- paste(unlist(Args), collapse = ", ")


                #Getting key
                key <- load_and_filter_key_for_current_version(path_to_key = path_to_key)


                #Filtering for the concept and amended the KEY_WORD and binding it to all other records
                add_to_key <-
                        key %>%
                        dplyr::filter_at(vars(IDENTITY_ID), dplyr::all_vars(. == identity_id)) %>%
                        dplyr::filter_at(vars(KEY_FIELD), dplyr::all_vars(. == key_field)) %>%
                        dplyr::filter_at(vars(KEY_CONCEPT_NAME), dplyr::all_vars(. == key_concept_name))

                if (nrow(add_to_key) > 0) {
                        add_to_key <-
                                add_to_key %>%
                                dplyr::filter(row_number() == 1) %>%
                                dplyr::mutate(KEY_WORD = paste0(KEY_WORD, ", ", new_key_words)) %>%
                                dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp())
                } else {
                        key_var <- enquo(key_field)
                        identity <- readr::read_csv("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/IDENTITY.csv", col_types = cols(.default = "c")) %>%
                                                dplyr::filter(IDENTITY_ID == identity_id) %>%
                                                dplyr::filter_at(vars(!!key_var), all_vars(. == key_concept_name)) %>%
                                                dplyr::filter(row_number() == 1) %>%
                                                dplyr::select(-IDENTITY_TIMESTAMP)

                        add_to_key <-
                                dplyr::tibble(IDENTITY_ID = identity_id,
                                              KEY_FIELD = key_field,
                                              KEY_CONCEPT_NAME = key_concept_name,
                                              KEY_WORD = new_key_words) %>%
                                dplyr::left_join(identity) %>%
                                dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp())

                }


                ##Adding new record to key
                current_key <-
                        readr::read_csv(path_to_key, col_types = cols(.default = "c"))

                new_key <-
                        dplyr::bind_rows(current_key,
                                         add_to_key)

                if ((nrow(new_key) - nrow(current_key)) != 1) {
                        typewriteR::tell_me("Warning: difference between the new key and current key is not 1.")
                        typewriteR::stop_and_enter()
                }

                readr::write_csv(new_key,
                                 path = path_to_key)

                if (!is.null(path_to_local_repo)) {
                        mirCat::git_add_all(path_to_local_repo = path_to_local_repo)
                        mirCat::git_commit(path_to_local_repo = path_to_local_repo,
                                           commit_message = "+: key_word/s to key")
                        mirCat::git_push_to_msk(path_to_local_repo = path_to_local_repo)
                }
        }
