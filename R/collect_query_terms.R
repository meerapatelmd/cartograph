#' Interactively collect phrases that will be used in the WHERE clause in the SQL statement
#' @param input_dataframe dataframe in the KEY format that contains the column of source concepts that need to be mapped
#' @param output_dataframe_name character vector of length 1 that will be the name of the output data, which will be list split based on concept and additional column of the new terms
#' @param script_step_number the number assigned to the script this function is being run on. This is important because the R object will be saved with the script number as a prefix
#' @import mirCat
#' @import crayon
#' @import typewriteR
#' @export


collect_query_terms <-
        function(input_dataframe, output_data_name, script_step_number) {
                if (!(exists(output_data_name, envir = globalenv()))) {
                        output_data <- list()
                        assign(output_data_name, output_data, envir = globalenv())
                        start_index <- 1
                } else {
                        output_data <- get(output_data_name, envir = globalenv())
                        start_index <- mirCat::list_last(output_data)
                }


                for (i in start_index:nrow(input_dataframe)) {
                        ##Each output_data list entry will itself be a list of the "IDENTITY" information and the dataframe of SQL_QUERY_TERMS
                        output_data[[i]] <- list(IDENTITY = list(),
                                                 SQL_QUERY_TERMS = data_frame())


                        ##First retreiving the identity information
                        identity_id <- input_dataframe$IDENTITY_ID[i]
                        key_field <- input_dataframe$KEY_FIELD[i]
                        key_concept_name <- input_dataframe$KEY_CONCEPT_NAME[i]

                        if (key_field != "FIELD_LABEL") {
                                output_data[[i]][["IDENTITY"]] <- cartographR::lookup_native_attributes(identity_id = identity_id,
                                                                                                        key_field = key_field,
                                                                                                        key_concept_name = key_concept_name)
                        } else {
                                output_data[[i]][["IDENTITY"]] <- list()
                        }


                        ##Saving RDS
                        rds_fn <- paste0(script_step_number, "_", output_data_name, ".RData")
                        saveRDS(output_data, file = rds_fn)
                        typewriteR::tell_me(Sys.time(), rds_fn, "saved...")
                        cat("\n")

                        ##Concurrent to saving RDS, assigning robject to global
                        assign(output_data_name, output_data, envir = globalenv())
                        typewriteR::tell_me(output_data_name, "R object updated...")
                        cat("\n")

                        ##Printing IDENTITY to console
                        typewriteR::tell_me(crayon::bold("IDENTITY INFORMATION:"))
                        cat("\n")
                        print(output_data[[i]][["IDENTITY"]])
                        cat("\n")

                        ##Pause to continue
                        typewriteR::tell_me("Please enter the SQL query terms next.")
                        cat("\n")
                        typewriteR::stop_and_enter()
                        cat("\n")

                        ##Collecting the search queries
                        x <- readline("Please enter the phrases separated by comma to split into single queries: ")
                        x <- stringr::str_trim(unlist(strsplit(x, split = ",")), "both")

                        if (length(x) != 0) {
                                data <- list()
                                for (j in 1:length(x)) {
                                        data[[j]] <-
                                                input_dataframe %>%
                                                dplyr::filter(row_number() == i) %>%
                                                dplyr::mutate(QUERY_TERM = x[j])
                                }
                                output_data[[i]][["SQL_QUERY_TERMS"]] <- dplyr::bind_rows(data) %>%
                                        dplyr::distinct()
                        } else {
                                output_data[[i]][["SQL_QUERY_TERMS"]] <- data.frame()
                        }


                        ##Save RDS a second time
                        saveRDS(output_data, file = rds_fn)
                        typewriteR::tell_me(Sys.time(), rds_fn, "saved...")
                        cat("\n")

                        ##Concurrent to saving RDS, assigning robject to global
                        assign(output_data_name, output_data, envir = globalenv())
                        typewriteR::tell_me(output_data_name, "R object updated...")
                        cat("\n")
                }

        }
