#' Query based on search terms the funnels in the order it is inputted and that does not write to catalogue
#' @param ... vector of phrases to collectively feed into the LIKE sql statement
#' @return resultset as a dataframe with all column types as character and trimmed white space
#' @importFrom mySeagull connect_to_local_postgres
#' @import DBI
#' @importFrom somersaulteR call_mr_clean
#' @import dplyr
#' @export

funnel_ask_athena <-
        function(...) {
                Args <- list(...)

                for (i in 1:length(Args)) {
                        if (i == 1) {
                                sql_statement <- paste0("SELECT * FROM public.concept WHERE concept_name LIKE '%", Args[[1]], "%';")
                                conn_to_athena <- mySeagull::connect_to_local_postgres(dbname = "athena")

                                resultset <- DBI::dbGetQuery(conn = conn_to_athena,
                                                             statement = sql_statement)

                                DBI::dbDisconnect(conn_to_athena)

                        } else {

                                resultset <-
                                        resultset %>%
                                        dplyr::filter_at(vars(concept_name), grepl(Args[[i]], ., ignore.case = TRUE) == TRUE)
                        }
                }

                return(resultset %>%
                               somersaulteR::call_mr_clean())
        }
