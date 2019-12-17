#' Returns MRCONSO NETS that match the phrase
#' @param phrase character vector of length 1.
#' @param ward_slash forward slashes throws errors with the write_csv and read_csv proceses downstream and are removed from the phrase by default
#' @param cast_wide_net TRUE if any NET with a grep logical match is to be included
#' @param list_by_cui selects only CUI and STR variables and splits output by CUI into a list
#' @import dplyr
#' @import readr
#' @import mirroR
#' @import typewriteR
#' @import stringr
#' @export

return_mrconso_net <-
        function(phrase,
                 path_to_lookup,
                 path_to_umls_net_dir = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/TerminologyBiblioTech/CATALOGUE/UMLS/NETS",
                 sub_forward_slash = TRUE,
                 trim_inner_space = TRUE,
                 sub_underscore = TRUE,
                 push_to_github = TRUE,
                 cast_wide_net = TRUE,
                 list_by_cui = TRUE) {

                lookup_fn <- path_to_lookup
                files_list <- list.files(path_to_umls_net_dir, full.names = TRUE)
                lookup <- readr::read_csv(lookup_fn, col_types = cols(.default = "c"))

                phrase_00 <- phrase
                if (sub_forward_slash == TRUE) {
                        phrase_01 <- stringr::str_replace_all(phrase_00, "[/]{1}", " ")
                } else {
                        phrase_01 <- phrase_00
                }

                if (sub_underscore == TRUE) {
                        phrase_01 <- stringr::str_replace_all(phrase_01, "[_]{1}", " ")
                } else {
                        phrase_01 <- phrase_01
                }

                if (trim_inner_space == TRUE) {
                        phrase_01 <- centipede::trimws(phase_01, which = "both")
                } else {
                        phrase_01 <- centipede::trimws(phase_01, which = "both", inner_space = FALSE)
                }

                exact_phrase_file_ext <- paste0("[0-9]{12}[_]{1}", phrase_01, "[.]{1}csv$")

                if (cast_wide_net == TRUE) {
                        phrase_02 <- phrase_01
                } else {
                        phrase_02 <- paste0("^", phrase_01, "$")
                }

                if (any(grepl(exact_phrase_file_ext, files_list)) == TRUE) {
                                fns <- grep(phrase_02, files_list, value = TRUE, ignore.case = TRUE)
                                if (length(fns) > 0) {
                                        x <- lapply(fns, readr::read_csv, col_types = cols(.default = "c"))
                                        x <- dplyr::bind_rows(x)

                                        if (nrow(x) > 0) {

                                                if (list_by_cui == TRUE) {
                                                        x <- split(x, x$CUI)
                                                        cuis <- names(x)
                                                        x <- lapply(1:length(x), function(i) x[[i]] %>% dplyr::select(STR) %>% dplyr::distinct() %>% unlist() %>% unname())
                                                        names(x) <- cuis
                                                        return(x)
                                                } else {
                                                        return(x)
                                                }

                                        } else {
                                                typewriteR::tell_me("\tInput phrase:", phrase)
                                                cat("\n")
                                                typewriteR::tell_me("\tPhrase 01:", phrase_01)
                                                cat("\n")
                                                typewriteR::tell_me("\tPhrase 02:", phrase_02)
                                                cat("\n")
                                                typewriteR::tell_me("\t\tNo match to CUIs in MRCONSO.")
                                                cat("\n")
                                                return(x)
                                                }
                                }
                } else {

                        write_mrconso_net_from_vector(phrases = phrase,
                                                      path_to_lookup = path_to_lookup,
                                                      path_to_umls_net_dir = path_to_umls_net_dir,
                                                      sub_forward_slash = sub_forward_slash,
                                                      trim_inner_space = trim_inner_space,
                                                      sub_underscore = sub_underscore,
                                                      push_to_github = push_to_github
                                                      )

                        lookup_fn <- path_to_lookup
                        files_list <- list.files(path_to_umls_net_dir, full.names = TRUE)
                        lookup <- readr::read_csv(lookup_fn, col_types = cols(.default = "c"))
                        phrase_00 <- phrase
                        if (sub_forward_slash == TRUE) {
                                phrase_01 <- stringr::str_replace_all(phrase_00, "[/]{1}", " ")
                        } else {
                                phrase_01 <- phrase
                        }

                        exact_phrase_file_ext <- paste0("[0-9]{12}[_]{1}", phrase_01, "[.]{1}csv$")

                        if (cast_wide_net == TRUE) {
                                phrase_02 <- phrase_01
                        } else {
                                phrase_02 <- paste0("^", phrase_01, "$")
                        }

                        fns <- grep(phrase_02, files_list, value = TRUE, ignore.case = TRUE)
                        x <- lapply(fns, readr::read_csv, col_types = cols(.default = "c"))
                        x <- dplyr::bind_rows(x)

                        if (nrow(x) > 0) {

                                if (list_by_cui == TRUE) {
                                        x <- split(x, x$CUI)
                                        cuis <- names(x)
                                        x <- lapply(1:length(x), function(i) x[[i]] %>% dplyr::select(STR) %>% dplyr::distinct() %>% unlist() %>% unname())
                                        names(x) <- cuis
                                        return(x)
                                } else {
                                        return(x)
                                }

                        } else {
                                typewriteR::tell_me("\tInput phrase:", phrase)
                                cat("\n")
                                typewriteR::tell_me("\tPhrase 01:", phrase_01)
                                cat("\n")
                                typewriteR::tell_me("\tPhrase 02:", phrase_02)
                                cat("\n")
                                typewriteR::tell_me("\t\tNo match to CUIs in MRCONSO.")
                                cat("\n")
                                return(x)
                        }

                }

        }


