#' Interactively collect keywords associated with a concept that will be used in the WHERE clause in the SQL statement
#' @param dataframe dataframe the column of source concepts that need to be mapped
#' @param output_dataframe_name character vector of length 1 that will be the name of the output data, which will be list split based on concept and additional column of the new terms
#' @param script_step_number the number assigned to the script this function is being run on. This is important because the R object will be saved with the script number as a prefix. If it is NULL, then it will not be saved.
#' @return original dataframe with a KEY_WORD column as a comma-separated string
#' @import mirCat
#' @import crayon
#' @importFrom readr read_rds
#' @import typewriteR
#' @export


collect_key_words <-
        function(dataframe, script_step_number, output_dataframe_name) {

                rds_fn <- paste0(script_step_number, "_", output_dataframe_name, ".RData")

                if (file.exists(rds_fn)) {
                        output_data <- readr::read_rds(rds_fn)
                        start_index <- nrow(output_data)

                } else {
                        output_data <- dataframe %>%
                                dplyr::mutate(KEY_WORD = "")
                        output_data <- output_data[-(1:nrow(output_data)),]
                        saveRDS(output_data, file = rds_fn)

                        start_index <- 1
                }

                for (i in start_index:nrow(dataframe)) {

                        print(as.list(dataframe[i,]))

                        ##Collecting the search queries
                        x <- readline("Please enter the phrases separated by comma to split into single queries. `S` to skip: ")
                        x <- stringr::str_trim(x, "both")

                        if (x != "S") {
                                output_data <-
                                        dplyr::bind_rows(output_data,
                                                         dataframe %>%
                                                                 dplyr::filter(row_number() == i) %>%
                                                                 dplyr::mutate(KEY_WORD = x) %>%
                                                                 somersaulteR::add_timestamp_column(new_col_name = "KEY_TIMESTAMP")
                                                        )

                                ##Saving RDS
                                saveRDS(output_data, file = rds_fn)
                                typewriteR::tell_me(Sys.time(), rds_fn, "saved...")
                                cat("\n")

                                ##Returning output
                                return(output_data)

                        } else {
                                output_data <-
                                        dplyr::bind_rows(output_data,
                                                         dataframe %>%
                                                                 dplyr::filter(row_number() == i) %>%
                                                                 dplyr::mutate(KEY_WORD = NA) %>%
                                                                 somersaulteR::add_timestamp_column(new_col_name = "KEY_TIMESTAMP")
                                                        )


                                ##Saving RDS
                                saveRDS(output_data, file = rds_fn)
                                typewriteR::tell_me(Sys.time(), rds_fn, "saved...")
                                cat("\n")

                                ##Returning output
                                return(output_data)
                        }
                }
        }
