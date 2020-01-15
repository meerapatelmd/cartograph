#' Searches local library concepts in key by only searching within variables marked with a "LOCAL" prefix
#' @param path_to_key path to key file. Key must contain local concepts columns.
#' @param string string to search
#' @param type whether to search an exact match for the string or not.
#' @importFrom readr read_csv
#' @import dplyr
#' @export
#'

search_key_as_local_library <-
        function(path_to_key,
                 string,
                 type = c("like", "exact")) {

                key_data <-
                        readr::read_csv(path_to_key,
                                        col_types = cols(.default = "c"))

                if (type == "exact") {
                        string <- paste0("^", string, "$")
                } else {
                        string <- string
                }

                output <-
                        key_data %>%
                        dplyr::filter_at(vars(dplyr::starts_with("LOCAL")), any_vars(grepl(string, ., ignore.case = TRUE)))

                return(output)
        }
