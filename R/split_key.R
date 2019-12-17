#' Splits KEY into PRECOORDINATED and ONE_TO_ONE or VARIABLE and PERMISSIBLE_VALUE
#' @param key_df KEY$CURRENT or KEY$HISTORY to split
#' @param by "mapping ratio" if a list of PRECOORDINATED vs ONE_TO_ONE is desired. "key field" if a variable vs. permissible_value split is desired.
#' @return list of key_df split into 2 dataframes
#' @importFrom crayon red
#' @import dplyr
#' @export


split_key <-
        function(key_df,
                 by = c("mapping ratio", "key field")) {
                output <- list()
                
                if (by == "mapping ratio") {
                        output[[1]] <-
                                current_key_df %>%
                                dplyr::group_by(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME) %>%
                                dplyr::mutate(OBS = length(KEY_CONCEPT_NAME)) %>%
                                dplyr::filter(OBS > 1) %>%
                                dplyr::ungroup() %>%
                                dplyr::select(-OBS)
                        
                        output[[2]] <-
                                current_key_df %>%
                                dplyr::group_by(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME) %>%
                                dplyr::mutate(OBS = length(KEY_CONCEPT_NAME)) %>%
                                dplyr::filter(OBS == 1) %>%
                                dplyr::ungroup() %>%
                                dplyr::select(-OBS)
                        
                        names(output) <- c("PRECOORDINATED", "ONE_TO_ONE")
                        return(output)
                } else if (by == "key field") {
                        output[[1]] <-
                                current_key_df %>%
                                dplyr::filter_at(vars(contains("PERMISSIBLE_VALUE")), all_vars(is.na(.)))
                        
                        output[[2]] <-
                                current_key_df %>%
                                dplyr::filter_at(vars(contains("PERMISSIBLE_VALUE")), all_vars(!is.na(.)))
                        
                        names(output) <- c("VARIABLE", "PERMISSIBLE_VALUE")
                        return(output)
                } else {
                        typewriteR::tell_me(crayon::red("ERROR:", by, "is not a valid argument."))
                }

        }
