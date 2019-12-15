#'Search key for keyword
#'@import dplyr
#'@export


search_key <-
        function(path_to_key,
                 keyword) {
                
                key <- read_key(path_to_key = path_to_key,
                                project_alias = "all")

                return(
                        key %>%
                                dplyr::filter_all(any_vars(grepl(keyword, ., ignore.case = TRUE) == TRUE))
                )
        }
