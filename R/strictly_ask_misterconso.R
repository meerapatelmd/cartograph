#' Query based on search terms
#' @importFrom mySeagull get_query
#' @importFrom stringr str_pad
#' @export

strictly_ask_misterconso <-
        function(phrase, limit = 1) {
                                sql_statement <- paste0("SELECT CUI, STR FROM MRCONSO WHERE LAT = 'ENG' AND ISPREF = 'Y' AND STR ='", phrase, "' ")
                                sql_statement <- paste0(sql_statement, "LIMIT ", limit, ";")
                resultset <- mySeagull::get_query("umls", sql_statement = sql_statement)
                return(resultset)
        }
