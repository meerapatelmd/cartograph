#'Searches a given lookup for a keyword
#'@import dplyr
#'@export

search_lookup <-
        function(path_to_lookup,
                 keyword) {
                
                lookup <- read_lookup(path_to_lookup = path_to_lookup)

                return(
                        lookup %>%
                        dplyr::filter_all(any_vars(grepl(keyword, ., ignore.case = TRUE) == TRUE))
                )
                
        }




