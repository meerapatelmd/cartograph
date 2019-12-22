#' Writes a MRCONSO NET file if one does not exist for the given phrase
#' @import dplyr
#' @import mirCat
#' @import readr
#' @import mirroR
#' @import typewriteR
#' @import mySeagull
#' @import crayon
#' @importFrom gmp as.bigz
#' @export

write_mrconso_net_from_vector <-
        function(phrases, path_to_lookup, path_to_umls_net_dir, sub_forward_slash = TRUE, trim_inner_space = TRUE, sub_underscore = TRUE, push_to_github = TRUE) {

                ##Reporting metrics
                typewriteR::tell_me(crayon::blue("\tThis vector has", length(phrases), "phrase/s and there are", unique(length(phrases)), "unique phrases to query."))


                ##Getting the lookup filename
                lookup_fn <- path_to_lookup
                lookup <- readr::read_csv(lookup_fn, col_types = cols(.default = "c"))

                ##Get a list of existing nets
                files_list <- list.files(path_to_umls_net_dir, full.names = TRUE)

                ##Iterating over phrases vector
                for (i in 1:length(phrases)) {
                        if (i == 1) {
                                total_obs <- length(phrases)
                        }

                        typewriteR::tell_me("\t", Sys.time(), "Starting", i, "of", total_obs, "phrases.")
                        typewriteR::tell_me("\t", "phrase:", phrases[i])

                        phrase_00 <- phrases[i]
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
                                phrase_01 <- centipede::trimws(phrase_01, which = "both")
                        } else {
                                phrase_01 <- centipede::trimws(phrase_01, which = "both", inner_space = FALSE)
                        }

                        exact_phrase_file_ext <- paste0("[0-9]{12}[_]{1}", phrase_01, "[.]{1}csv$")
                        phrase_02 <- paste0("^", phrase_01, "$")

                        if (any(grepl(exact_phrase_file_ext, files_list)) == FALSE) {

                                        typewriteR::tell_me("\t", phrases[i], "not in existing UMLS NETS.")
                                        typewriteR::tell_me("\t", Sys.time(), "Querying MRCONSO...")
                                        # projektoR::append_csv(lookup_fn,
                                        #                       dataframe = data.frame(
                                        #                               UMLS_SQL_KEYWORD_TIMESTAMP = mirroR::get_timestamp(),
                                        #                               UMLS_SQL_KEYWORD_ID =  generate_next_id_number(lookup$UMLS_SQL_KEYWORD_ID),
                                        #                               UMLS_SQL_KEYWORD = phrase_01 #Not `phrase_02` because the regex will not be compatible with SQL QUERIES
                                        #                       ))
                                        #
                                        # lookup     <- readr::read_csv(lookup_fn, col_types = cols(.default = "c"))
                                        # keyword_id <- lookup %>%
                                        #                         dplyr::filter(UMLS_SQL_KEYWORD == phrase_01) %>%
                                        #                         dplyr::select(UMLS_SQL_KEYWORD_ID) %>%
                                        #                         dplyr::filter(row_number() == 1) %>%
                                        #                         unlist() %>%
                                        #                         unname()


                                        sql_statement <- paste0("SELECT * FROM MRCONSO WHERE LAT = 'ENG' AND ISPREF = 'Y' AND STR LIKE '%", phrase_01, "%';") #Not `phrase_02` because the regex will not be compatible with SQL QUERIES
                                        cohort        <- mySeagull::get_query("umls", sql_statement)

                                        sql_statement <- paste0("SELECT * FROM MRCONSO WHERE LAT = 'ENG' AND ISPREF = 'Y' AND STR = '", phrase_01, "' LIMIT 1;")
                                        coordinate <- mySeagull::get_query("umls", sql_statement)

                                        keyword_id <- centipede::calculate_next_integer(lookup$UMLS_SQL_KEYWORD_ID)

                                        add_to_lookup <-
                                                data.frame(UMLS_SQL_KEYWORD_TIMESTAMP = mirroR::get_timestamp(),
                                                           UMLS_SQL_KEYWORD_ID =  keyword_id,
                                                           UMLS_SQL_KEYWORD = phrase_01) %>% #Not `phrase_02` because the regex will not be compatible with SQL QUERIES
                                                        dplyr::mutate(COORDINATE_CUI = ifelse(nrow(coordinate) == 1, coordinate$CUI, NA)) %>%
                                                        dplyr::mutate(COORDINATE_STR = ifelse(nrow(coordinate) == 1, coordinate$STR, NA)) %>%
                                                        dplyr::mutate(NET_CUI_COUNT = length(unique(cohort$CUI))) %>%
                                                        dplyr::mutate(NET_CUI_COUNT = nrow(cohort)) %>%
                                                somersaulteR::call_mr_clean()


                                        mirCat::append_csv(lookup_fn,
                                                              dataframe = add_to_lookup
                                        )

                                        ##Refreshing lookup object with new version of lookup
                                        lookup <- readr::read_csv(lookup_fn, col_types = cols(.default = "c"))

                                        output_fn <- mirroR::create_path_to_file(path_folder = path_to_umls_net_dir,
                                                                          basename = paste0(keyword_id, "_", phrase_01), #Not `phrase_02` because the regex will not be compatible with output filenames
                                                                          file_extension = "csv")

                                        if (!(file.exists(output_fn))) {
                                                typewriteR::tell_me(crayon::yellow("\t", "Filename:", basename(output_fn)))
                                                typewriteR::tell_me("\t", Sys.time(), "Starting to write csv.")
                                                readr::write_csv(cohort, path = output_fn)
                                                typewriteR::tell_me("\t", Sys.time(), "Successfully wrote csv.")
                                                typewriteR::tell_me("\t", Sys.time(), "Ending", i, "of", total_obs, ".")
                                                cat("\n\n")


                                                if (push_to_github == TRUE) {
                                                        mirCat::git_add_all(path_to_local_repo = path_to_bibliotech_repo)
                                                        mirCat::git_commit(path_to_local_repo = path_to_bibliotech_repo,
                                                                           commit_message = paste0("+: ", phrase_00, " to UMLS lookup."))
                                                        mirCat::git_push_to_msk(path_to_local_repo = path_to_bibliotech_repo)

                                                        cat("\n\n\n")
                                                }
                                        }

                        } else {
                                typewriteR::tell_me("\t", phrases[i], "has been found in the existing UMLS NETS.")
                                typewriteR::tell_me("\t", Sys.time(), "Ending", i, "of", total_obs, "phrases.")
                                cat("\n\n\n")
                        }
                }
        }


