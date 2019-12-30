#' Query based on search terms that does not write to catalogue
#' @param all_columns default is FALSE and means only CUI and STR will be returned. If TRUE, returns all the MRCONSO tables
#' @param limit default is NULL and returns full resultset.
#' @importFrom mySeagull get_query
#' @import dplyr
#' @import rubix
#' @export

loosely_ask_misterconso <-
        function(..., limit = NULL, all_columns = FALSE) {
                Args <- list(...)

                if (all_columns == FALSE) {
                        for (i in 1:length(Args)) {
                                if (i == 1) {
                                        sql_statement <- paste0("SELECT CUI, STR FROM MRCONSO WHERE LAT = 'ENG' AND ISPREF = 'Y' AND STR LIKE '%", Args[[1]], "%'")
                                } else {
                                        sql_statement <- paste0(sql_statement,
                                                                paste0(" AND STR LIKE '%", Args[[i]], "%'"))
                                }
                        }

                        if (!is.null(limit)) {
                                sql_statement <- paste0(sql_statement, " LIMIT ", limit)
                        } else {
                                sql_statement <- sql_statement
                        }
                } else {
                        for (i in 1:length(Args)) {
                                if (i == 1) {
                                        sql_statement <- paste0("SELECT * FROM MRCONSO WHERE LAT = 'ENG' AND ISPREF = 'Y' AND STR LIKE '%", Args[[1]], "%'")
                                } else {
                                        sql_statement <- paste0(sql_statement,
                                                                paste0(" AND STR LIKE '%", Args[[i]], "%'"))
                                }
                        }

                        if (!is.null(limit)) {
                                sql_statement <- paste0(sql_statement, " LIMIT ", limit)
                        } else {
                                sql_statement <- sql_statement
                        }
                }

                sql_statement <- paste0(sql_statement, ";")
                resultset <- mySeagull::get_query("umls", sql_statement = sql_statement)
                return(resultset %>%
                               rubix::call_mr_clean())
        }
