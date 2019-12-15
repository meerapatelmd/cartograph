# convert_to_casted_format <-
#         function(dataframe, target_colnames = c("VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL"), na.rm = TRUE) {
#                 x <- reshape2::dcast(dataframe, IDENTITY_ID+KEY_CONCEPT_NAME ~ KEY_FIELD, value.var = "KEY_CONCEPT_NAME", na.rm = na.rm) %>%
#                         dplyr::select(-KEY_FIELD) %>%
#                         dplyr::left_join(dataframe) %>%
#                         dplyr::filter_at(vars(target_colnames), any_vars(!(is.na(.)))) %>%
#                         dplyr::select(KEY_TIMESTAMP, IDENTITY_ID, dplyr::everything())
#                 return(x)
#         }
