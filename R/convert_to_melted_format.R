
# convert_to_melted_format <-
#         function(dataframe, target_colnames = c("VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL"), na.rm = TRUE) {
#                 x <- reshape2::melt(dataframe, measure.vars = target_colnames,
#                                     variable.name = "KEY_FIELD",
#                                     value.name = "KEY_CONCEPT_NAME",
#                                     na.rm = na.rm)
#
#                 return(x)
#         }
