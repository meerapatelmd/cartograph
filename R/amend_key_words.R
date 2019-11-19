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
        function(concept_col, concept_to_amend, key_words_to_add, script_step_number, output_dataframe_name) {

                #Prepping columns
                concept_col <- enquo(concept_col)

                #Getting rds_fn
                rds_fn <- paste0(script_step_number, "_", output_dataframe_name, ".RData")
                output_data <- readr::read_rds(rds_fn) %>%
                                        somersaulteR::call_mr_clean()

                #Filtering for the concept and amended the KEY_WORD and binding it to all other records
                new_output_data <-
                        dplyr::bind_rows(output_data %>%
                                                 dplyr::filter_at(vars(!!concept_col), dplyr::all_vars(. != concept_to_amend)),
                                        output_data %>%
                                                dplyr::filter_at(vars(!!concept_col), dplyr::all_vars(. == concept_to_amend)) %>%
                                                dplyr::mutate(KEY_WORD = paste0(KEY_WORD, ",", key_words_to_add)) %>%
                                                dplyr::mutate(KEY_TIMESTAMP = mirroR::get_timestamp())
                                        )

                brake_if_nrows_dont_match(output_data, new_output_data)

                ##Saving RDS
                saveRDS(output_data, file = rds_fn)
                typewriteR::tell_me(Sys.time(), rds_fn, "saved...")
                cat("\n")

                typewriteR::tell_me("Object", output_dataframe_name, "will now be overwritten.")
                typewriteR::stop_and_enter()
                assign(output_dataframe_name, output_data, envir = globalenv())
        }
