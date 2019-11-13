#' Parses 'yesno' field types as permissible values
#' @import dplyr
#' @export
#'
parse_redcap_boolean_values <-
        function(dataframe, id_col, variable_col, field_type_col, new_permissible_col_name = "PERMISSIBLE_VALUE_LITERAL") {
                id_col <- enquo(id_col)
                variable_col <- enquo(variable_col)
                field_type_col <- enquo(field_type_col)
                new_permissible_col_name <- enquo(new_permissible_col_name)

                pv.id        <- vector()
                pv.literal <- vector()
                pv.names <- vector()

                for (i in 1:nrow(dataframe)) {
                        id       <- dataframe %>%
                                dplyr::select(!!id_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()

                        variable <- dataframe %>%
                                dplyr::select(!!variable_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()

                        field_type <-
                                dataframe %>%
                                dplyr::select(!!field_type_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist()

                        if (field_type %in% c("yesno")) {
                                final.pv.literal_of_i <- c("1, yes", "2, no")

                                pv.id <- c(pv.id, rep_len(id, length.out = length(final.pv.literal_of_i)))
                                pv.literal <-  c(pv.literal, final.pv.literal_of_i)
                                pv.names <- c(pv.names, rep_len(variable,
                                                                length.out = length(final.pv.literal_of_i)))
                        }
                }

                x <- data.frame(pv.id = pv.id,
                                pv.literal = pv.literal,
                                pv.names = pv.names
                )

                x <-
                        x %>%
                        dplyr::rename(!!id_col := pv.id,
                                      !!new_permissible_col_name := pv.literal,
                                      !!variable_col := pv.names)

                return(x)
        }


