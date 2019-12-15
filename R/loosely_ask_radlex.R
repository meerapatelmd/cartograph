#' Queries RadLex.xls as if it is a database
#' @import readxl
#' @import dplyr
#' @export

loosely_ask_radlex <-
        function(phrase, path_to_radlex_xls = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/RadLex_v4/Radlex.xls") {
                radlex <- readxl::read_xls(path_to_radlex_xls, col_types = "text")
                updated_radlex <- radlex %>%
                        dplyr::filter(Obsolete == FALSE)
                
                output <-
                        updated_radlex %>%
                        dplyr::filter_all(any_vars(grepl(phrase, ., ignore.case = TRUE)))
                
                return(output)
        }


