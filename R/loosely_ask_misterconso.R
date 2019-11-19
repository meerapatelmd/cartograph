#' Query based on search terms that does not write to catalogue
#' @importFrom mySeagull get_query
#' @import dplyr
#' @importFrom somersaulteR call_mr_clean
#' @export

loosely_ask_misterconso <-
        function(...) {
                Args <- list(...)

                for (i in 1:length(Args)) {
                        if (i == 1) {
                                sql_statement <- paste0("SELECT CUI, STR FROM MRCONSO WHERE LAT = 'ENG' AND ISPREF = 'Y' AND STR LIKE '%", Args[[1]], "%'")
                        } else {
                                sql_statement <- paste0(sql_statement,
                                                   paste0(" AND STR LIKE '%", Args[[i]], "%'"))
                        }
                }
                sql_statement <- paste0(sql_statement, ";")
                resultset <- mySeagull::get_query("umls", sql_statement = sql_statement)
                return(resultset %>%
                               somersaulteR::call_mr_clean())
        }
