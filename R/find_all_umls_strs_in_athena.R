#' This function takes all the possible strings associated with a UMLS cui, splits each individual string into tokens and performs a "LIKE" query on the combination of tokens per string in athena
#' @param input_df Input dataframe should have a minimum of 2 columns. One of the CUI and the 2nd of the label for the cui, which can be either an exact string associated with the CUI or another label
#' @import metaorite
#' @import DBI
#' @importFrom mySeagull connect_to_local_postgres
#' @import dplyr
#' @import centipede
#' @export


find_all_umls_strs_in_athena <-
        function(input_df, cui_col, label_col) {

                ##Preparing cui and str/col
                cui_col <- enquo(cui_col)
                label_col <- enquo(label_col)

                ##Selecting and getting distinct values per observation
                input_df <-
                        input_df %>%
                        dplyr::select(!!cui_col, !!label_col) %>%
                        dplyr::distinct()

                output <- list()
                for (i in 1:nrow(input_df)) {
                        cui <- input_df %>%
                                dplyr::select(!!cui_col) %>%
                                dplyr::filter(row_number() == i) %>%
                                unlist() %>%
                                unname()

                        all_strs <- metaorite::query_umls_statement(paste0("SELECT DISTINCT STR FROM MRCONSO WHERE CUI = '", cui, "';"))
                        all_strs <-
                                all_strs %>%
                                unlist() %>%
                                unname()

                        for (j in 1:length(all_strs)) {
                                str <- all_strs[j]
                                str_vector <- unlist(centipede::strsplit(str, split = " "))

                                Args <- as.list(str_vector)

                                for (k in 1:length(Args)) {
                                        if (k == 1) {
                                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%",
                                                                        Args[[1]], "%'")
                                        }
                                        else {
                                                sql_statement <- paste0(sql_statement, paste0(" AND concept_name LIKE '%",
                                                                                              Args[[k]], "%'"))
                                        }
                                }

                                sql_statement <- paste0(sql_statement, ";")
                                print(sql_statement)

                                if (j == 1) {
                                        conn_to_athena <- mySeagull::connect_to_local_postgres(dbname = "athena")
                                        resultset <- DBI::dbGetQuery(conn = conn_to_athena, statement = sql_statement)
                                        DBI::dbDisconnect(conn_to_athena)
                                } else {
                                        conn_to_athena <- mySeagull::connect_to_local_postgres(dbname = "athena")
                                        x <- DBI::dbGetQuery(conn = conn_to_athena, statement = sql_statement)
                                        DBI::dbDisconnect(conn_to_athena)

                                        resultset <- dplyr::bind_rows(resultset,
                                                                      x)
                                }
                        }

                        output[[i]] <- resultset
                        names(output)[[i]] <- input_df %>%
                                                dplyr::select(!!label_col) %>%
                                                dplyr::filter(row_number() == i) %>%
                                                unlist() %>%
                                                unname()
                        #mirCat::stop_before_continue()
                }
                return(output)
        }

