#' Reads lookup
#' @import readr
#' @export

read_lookup <-
        function(path_to_lookup) {
                
                return(readr::read_csv(path_to_lookup, col_types = readr::cols(.default = "c")))
        }
