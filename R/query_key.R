#'Queries key
#'@import dplyr
#'@importFrom readr read_csv
#'@export


query_key <-
        function(path_to_key,
                 concept_name_col = KEY_CONCEPT_NAME,
                 phrase_to_query) {

                concept_name_col <- enquo(concept_name_col)

                lookup <- readr::read_csv(path_to_lookup,
                                          col_types = cols(.default = "c"))

                output <-
                        lookup %>%
                        dplyr::filter_at(vars(!!concept_name_col),
                                         any_vars(grepl(phrase_to_query, ., ignore.case = TRUE) == TRUE))
                return(output)
        }
