#' Lists variables with each unique pv in a dataframe to a docx with given output_fn
#' @import officer
#' @import dplyr
#' @export

list_var_pvs_in_docx <-
        function(data, document_title = "", document_subtitle = "", output_fn) {
                output <-
                        lapply(1:ncol(data),
                               function(x) data %>%
                                       dplyr::select(x) %>%
                                       dplyr::distinct() %>%
                                       dplyr::arrange() %>%
                                       unlist() %>%
                                       unname()) %>%
                        purrr::set_names(colnames(data))


                for (i in 1:length(output)) {
                        if (i == 1) {
                                output_document <-
                                        officer::read_docx() %>%
                                        officer::body_add_par(value = document_title, style = "heading 1") %>%
                                        officer::body_add_par(value = document_subtitle) %>%
                                        officer::body_add_par(value = paste0("Date and Time: ", mirCat::stamp_this())) %>%
                                        officer::body_add_par(value = "")

                                output_document <-
                                        output_document %>%
                                        officer::body_add_par(names(output)[i], style = "heading 2") %>%
                                        officer::body_add_par(paste(output[[i]], collapse = "|"))
                        } else {
                                output_document <-
                                        output_document %>%
                                        officer::body_add_par(names(output)[i], style = "heading 2") %>%
                                        officer::body_add_par(paste(output[[i]], collapse = "|"))
                        }
                }

                #Writing File
                new_docx_file <- print(output_document,
                                       target = output_fn)
        }

