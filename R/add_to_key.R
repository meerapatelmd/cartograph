#' Adds a new key data to the existing key as indicated by the path_to_key
#' @importFrom mirCat append_csv
#' @importFrom typewriteR tell_me
#' @importFrom typewriteR stop_and_enter
#' @import dplyr
#' @importFrom stringr str_replace_all
#' @importFrom mirCat get_timestamp
#' @export

add_to_key <-
        function(path_to_key,
                 dataframe,
                 key_type = c("umls", "omop")) {

                stopifnot(length(key_type) == 1)

                if (key_type == "umls") {

                        typewriteR::tell_me("Will now append the input after updating the timestamp and overwriting MSK_CONCEPT_ID using the CUI column.")
                        typewriteR::stop_and_enter()


                        mirCat::append_csv(csv_fn = path_to_key,
                                           dataframe = dataframe %>%
                                                   dplyr::mutate(KEY_TIMESTAMP = mirCat::get_timestamp()) %>%
                                                   dplyr::mutate(MSK_CONCEPT_ID = stringr::str_replace_all(CUI, "C", "MSK"))
                                           )
                } else if (key_type == "omop") {

                        typewriteR::tell_me("Will now append the input after updating the timestamp")
                        typewriteR::stop_and_enter()

                        mirCat::append_csv(csv_fn = path_to_key,
                                           dataframe = dataframe %>%
                                                   dplyr::mutate(KEY_TIMESTAMP = mirCat::get_timestamp()))
                } else {
                        typewriteR::tell_me(key_type, "is not a valid key type. Please try again.")
                }
        }