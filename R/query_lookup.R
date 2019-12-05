#'Queries lookup
#'@import dplyr
#'@importFrom readr read_csv
#'@export


query_lookup <-
        function(path_to_lookup,
                 keyword_col,
                 phrase_to_query) {

                keyword_col <- enquo(keyword_col)

                lookup <- readr::read_csv(path_to_lookup,
                                          col_types = cols(.default = "c"))

                output <-
                        lookup %>%
                        dplyr::filter_at(vars(!!keyword_col),
                                         any_vars(grepl(phrase_to_query, ., ignore.case = TRUE) == TRUE))
                return(output)
        }
