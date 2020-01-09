#' Search key for a term
#' @param path_to_key path to key file
#' @param string string to search
#' @importFrom readr read_csv
#' @import dplyr
#' @export
#'

search_key <-
        function(path_to_key,
                 string) {

                key_data <-
                        readr::read_csv(path_to_key,
                                        col_types = cols(.default = "c"))

                output <-
                        key_data %>%
                        dplyr::filter_all(any_vars(grepl(string, ., ignore.case = TRUE)))

                return(output)
        }
