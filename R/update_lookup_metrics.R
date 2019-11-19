# load("~/GitHub/MSK_KMI_Enterprise/MoonWalk/42.RData")
# glossary_fn <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS_MT_MRCONSO/GLOSSARY_UMLS_MT_MRCONSO_QUERY_TERMS.csv"
# lookup <- readr::read_csv(glossary_fn, col_types = cols(.default = "c"))
#
# if (!(exists("checklist", envir = globalenv()))) {
#         checklist <- list()
#         start_index <- 1
# } else if (mirCat::list_length(checklist) == 0) {
#         start_index <- 1
# } else {
#         start_index <- mirCat::list_length(checklist)
# }

#start_index <- 1172

# update_lookup_metrics <-
#         function(path_to_lookup) {
#                 checklist <- list()
#                 lookup <- readr::read_csv(path_to_lookup,
#                                           col_types = cols(.default = "c"))
#                 for (i in 1:nrow(lookup)) {
#                         if (i == 1) {
#                                 total_obs <- nrow(lookup)
#                         }
#
#                         cat("\n")
#                         typewriteR::tell_me(Sys.time(), "Starting", i, "of", total_obs)
#                         cat("\n")
#
#                         if (is.na(lookup$COORDINATE_CUI[i]) &
#                             is.na(lookup$COORDINATE_STR[i]) &
#                             is.na(lookup$NET_CUI_COUNT[i]) &
#                             is.na(lookup$NET_ROW_COUNT[i])
#                         ) {
#                                 cat("\n")
#                                 typewriteR::tell_me(Sys.time(), "Getting coordinate and net counts for", lookup$UMLS_SQL_KEYWORD[i])
#                                 #typewriteR::stop_and_enter()
#                                 cat("\n")
#
#                                 if ((grepl("^[0-9]{1,}$", lookup$UMLS_SQL_KEYWORD[i]) ==FALSE) & (nchar(lookup$UMLS_SQL_KEYWORD[i]) >2)) {
#
#
#
#                                         x <- return_mrconso_net(lookup$UMLS_SQL_KEYWORD[i], path_to_lookup = path_to_lookup, list_by_cui = FALSE)
#                                         x <-
#                                                 x %>%
#                                                 dplyr::distinct()
#                                         #return(x)
#                                         #typewriteR::stop_and_enter()
#
#                                         if (nrow(x) > 0) {
#                                                 print(x)
#                                                 typewriteR::stop_and_enter()
#                                                 checklist[[i]] <- x %>%
#                                                         dplyr::filter(LAT == "ENG", ISPREF == "Y") %>%
#                                                         dplyr::filter_at(vars(STR), all_vars(grepl(paste0("^", lookup$UMLS_SQL_KEYWORD[i], "$"), ., ignore.case = TRUE) == TRUE)) %>%
#                                                         dplyr::filter(row_number() == 1)
#                                         } else {
#                                                 checklist[[i]] <- x
#                                         }
#
#                                         cat("\n")
#                                         typewriteR::tell_me(Sys.time(), "Finished reading MRCONSO NET for", lookup$UMLS_SQL_KEYWORD[i])
#                                         cat("\n")
#
#                                         if (nrow(checklist[[i]]) == 1) {
#                                                 lookup$COORDINATE_CUI[i] <- checklist[[i]]$CUI
#                                                 lookup$COORDINATE_STR[i] <- checklist[[i]]$STR
#                                                 lookup$NET_CUI_COUNT[i] <- length(unique(x$CUI))
#                                                 lookup$NET_ROW_COUNT[i] <- nrow(x)
#                                         } else {
#                                                 lookup$COORDINATE_CUI[i] <- NA
#                                                 lookup$COORDINATE_STR[i] <- NA
#                                                 lookup$NET_CUI_COUNT[i] <- length(unique(x$CUI))
#                                                 lookup$NET_ROW_COUNT[i] <- nrow(x)
#                                         }
#
#                                         #typewriteR::stop_and_enter()
#
#                                         cat("\n")
#                                         typewriteR::tell_me(Sys.time(), "Updating lookup with", i, "of", total_obs)
#                                         cat("\n")
#                                         readr::write_csv(lookup, path_to_lookup)
#                                         cat("\n")
#                                         typewriteR::tell_me(Sys.time(), "Completed", i, "of", total_obs)
#                                         checklist[[i]] <- data.frame()
#                                         cat("\n")
#                                         cat("\n\n\n")
#                                 }
#                         }
#                 }
#
#         }
