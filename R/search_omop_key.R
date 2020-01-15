#' Search OMOP key for a term
#' @param path_to_key path to key file
#' @param string string to search
#' @importFrom readr read_csv
#' @import dplyr
#' @export
#'

search_omop_key <-
        function(path_to_key = "/Users/patelm9/GitHub/MSK/biblio-tech/KEY/REDCap/KEY_TO_OMOP.csv",
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
                        dplyr::filter_all(any_vars(grepl(string, ., ignore.case = TRUE)))

                return(output)
        }
