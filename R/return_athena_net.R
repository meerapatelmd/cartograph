#' Writes Athena CONCEPT table NET
#' @param phrases character vector of length 1 or greater of queries
#' @param path_to_athena_lookup path to lookup table
#' @param id_start_index if number of rows of the lookup is 0, the starting index for the id number assigned
#' @param path_to_repo if not NULL, will add, commit, and push each iterative change to origin master of the repo
#' @return csv files for each unique phrase queried in concept table and at the conclusion of the interation, returns the most updated Athena lookup to the console
#' @import readr
#' @import dplyr
#' @import mirCat
#' @importFrom typewriteR tell_me
#' @importFrom crayon cyan
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @importFrom mirroR create_path_to_file
#' @importFrom projektoR append_csv
#' @export


return_athena_net <-
        function(phrases,
                 path_to_athena_lookup = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Athena_Vocabulary_v5/LOOKUP.csv",
                 path_to_net_dir = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Athena_Vocabulary_v5/NETS",
                 sub_forward_slash = TRUE,
                 trim_inner_space = TRUE,
                 sub_underscore = TRUE,
                 id_start_index = 60000,
                 path_to_repo = NULL,
                 return_lookup = FALSE) {

                output <- list()
                for (i in 1:length(phrases)) {
                        athena_lookup <- readr::read_csv(path_to_athena_lookup,
                                                         col_types = cols(.default = "c"))

                        if (i == 1) {
                                total_obs <- length(phrases)
                        }

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
                                phrase_01 <- trimws(trimis(phrase_01), "both")
                        } else {
                                phrase_01 <- trimws(phrase_01, "both")
                        }

                        typewriteR::tell_me("Starting", i, "of", crayon::cyan(total_obs))
                        typewriteR::tell_me("Native Phrase:", crayon::cyan(phrase_00))
                        typewriteR::tell_me("Processed Phrase:", crayon::cyan(phrase_01))


                         if (!(phrase_01 %in% athena_lookup$ATHENA_SQL_KEYWORD)) {
                                if (nrow(athena_lookup) == 0) {
                                        athena_sql_keyword_id <- id_start_index
                                } else {
                                        athena_sql_keyword_id <- max(as.integer(athena_lookup$ATHENA_SQL_KEYWORD_ID)) + (sample(1:9, 1))
                                }

                                brake_if_na(athena_sql_keyword_id)

                                conn_to_athena <- mySeagull::connect_to_local_postgres(dbname = "athena")
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%", phrase_01, "%';")
                                x <- DBI::dbGetQuery(conn_to_athena, sql_statement)


                                output_fn <-
                                        mirroR::create_path_to_file(path_folder = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Athena_Vocabulary_v5/NETS",
                                                                    basename = paste0(athena_sql_keyword_id, "_", phrase_01),
                                                                    file_extension = "csv")


                                readr::write_csv(x, path = output_fn)

                                projektoR::append_csv(csv_fn = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Athena_Vocabulary_v5/LOOKUP.csv",
                                                      dataframe = dplyr::tibble(ATHENA_SQL_KEYWORD_TIMESTAMP = mirroR::get_timestamp(),
                                                                         ATHENA_SQL_KEYWORD_ID = athena_sql_keyword_id,
                                                                         ATHENA_SQL_KEYWORD = phrase_01,
                                                                         NET_CUI_COUNT = length(unique(x$concept_code)),
                                                                         NET_ROW_COUNT = nrow(x)) %>%
                                                                                somersaulteR::call_mr_clean()
                                                        )

                                DBI::dbDisconnect(conn_to_athena)

                                if (!(is.null(path_to_repo))) {
                                        mirCat::git_add_all(path_to_repo)
                                        mirCat::git_commit(path_to_repo, commit_message = paste0("+: ", paste0(athena_sql_keyword_id, "_", phrase_01)))
                                        mirCat::git_push_to_msk(path_to_repo)

                                        typewriteR::tell_me(i, "of", crayon::cyan(total_obs), "completed.")
                                        cat("\n\n\n")

                                        athena_lookup <- readr::read_csv(path_to_athena_lookup,
                                                                         col_types = cols(.default = "c"))

                                        fns <-
                                        athena_lookup %>%
                                                dplyr::filter_at(vars(ATHENA_SQL_KEYWORD), dplyr::all_vars(grepl(phrase_01, ., ignore.case = TRUE) == TRUE)) %>%
                                                dplyr::transmute(NET_FILENAME = paste0(path_to_net_dir, "/", ATHENA_SQL_KEYWORD_ID, "_", ATHENA_SQL_KEYWORD, ".csv")) %>%
                                                unlist() %>%
                                                unname()

                                        output[[i]] <- dplyr::bind_rows(lapply(fns, readr::read_csv, col_types = readr::cols(.default = "c"))) %>%
                                                        dplyr::distinct()
                                        names(output)[i] <- phrase_01

                                } else {
                                        typewriteR::tell_me(i, "of", crayon::cyan(total_obs), "completed.")
                                        cat("\n\n\n")

                                        athena_lookup <- readr::read_csv(path_to_athena_lookup,
                                                                         col_types = cols(.default = "c"))

                                        fns <-
                                                athena_lookup %>%
                                                dplyr::filter_at(vars(ATHENA_SQL_KEYWORD), dplyr::all_vars(grepl(phrase_01, ., ignore.case = TRUE) == TRUE)) %>%
                                                dplyr::transmute(NET_FILENAME = paste0(path_to_net_dir, "/", ATHENA_SQL_KEYWORD_ID, "_", ATHENA_SQL_KEYWORD, ".csv")) %>%
                                                unlist() %>%
                                                unname()

                                        output[[i]] <- dplyr::bind_rows(lapply(fns, readr::read_csv, col_types = readr::cols(.default = "c"))) %>%
                                                dplyr::distinct()
                                        names(output)[i] <- phrase_01
                                }

                        } else {
                                fns <-
                                        athena_lookup %>%
                                        dplyr::filter_at(vars(ATHENA_SQL_KEYWORD), dplyr::all_vars(grepl(phrase_01, ., ignore.case = TRUE) == TRUE)) %>%
                                        dplyr::transmute(NET_FILENAME = paste0(path_to_net_dir, "/", ATHENA_SQL_KEYWORD_ID, "_", ATHENA_SQL_KEYWORD, ".csv")) %>%
                                        unlist() %>%
                                        unname()

                                output[[i]] <- dplyr::bind_rows(lapply(fns, readr::read_csv, col_types = readr::cols(.default = "c"))) %>%
                                        dplyr::distinct()
                                names(output)[i] <- phrase_01

                        }
                }

                return(output)
        }


