

#
#
#
#
# lookup <- readr::read_csv(path_to_umls_lookup, col_types = cols(.default = "c"))
# new_lookup <- lookup
# for (i in 1:nrow(lookup)) {
#         if (grepl("2019-11-16", lookup$UMLS_SQL_KEYWORD_TIMESTAMP[i]) == TRUE) {
#                 fn <- paste0("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/UMLS/NETS",
#                              "/",
#                              lookup$UMLS_SQL_KEYWORD_ID[i],
#                              "_",
#                              lookup$UMLS_SQL_KEYWORD[i],
#                              ".csv")
#
#                 if (file.exists(fn)) {
#                         data <- readr::read_csv(fn,
#                                                 col_types = cols(.default = "c")
#                                                 )
#
#                         new_lookup$NET_CUI_COUNT[i] <- length(unique(data$CUI))
#
#                         new_lookup$NET_ROW_COUNT[i] <- nrow(data)
#
#                         data_coordinate <-
#                                 data %>%
#                                 dplyr::filter_at(vars(STR), all_vars(grepl(paste0("^", lookup$UMLS_SQL_KEYWORD[i], "$"), ., ignore.case = TRUE) == TRUE)) %>%
#                                 dplyr::select(CUI, STR) %>%
#                                 dplyr::filter(row_number() == 1) %>%
#                                 unlist()
#
#                         if (length(data_coordinate) == 2) {
#                                 new_lookup$COORDINATE_CUI[i] <- data_coordinate["CUI"]
#                                 new_lookup$COORDINATE_STR[i] <- data_coordinate["STR"]
#                         } else {
#                                 new_lookup$COORDINATE_CUI[i] <- NA
#                                 new_lookup$COORDINATE_STR[i] <- NA
#                         }
#
#                 }
#         }
# }
# #write_csv(new_lookup, path = path_to_umls_lookup)
