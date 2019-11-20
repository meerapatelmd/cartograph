#' Takes a key or dataframe joined with a key and divides it based on status mapped out in a cartograph
#' @param dataframe dataframe with the necessary columns mapped out in the function
#' @import dplyr
#' @importFrom rlang parse_expr
#' @export

cartograph_key <-
        function(dataframe) {
                cartograph <-
                        dplyr::bind_rows(
                                data.frame(bifurcation = "1",
                                           sequence = "1",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(CUI), any_vars(!is.na(.))",
                                           status = "MAPPED_TO_SINGLE_CUI",
                                           definition = "concept mapped to a single cui that fully encapsulates meaning of the native concept",
                                           parent = "PARENT"),
                                data.frame(bifurcation = "2",
                                           sequence = "1",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(CUI_FLAG), any_vars(!is.na(.))",
                                           status = "FLAGGED",
                                           definition = "concept mapped to a single cui that fully encapsulates meaning of the native concept",
                                           parent = "MAPPED_TO_SINGLE_CUI"),
                                data.frame(bifurcation = "2",
                                           sequence = "2",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(CUI_FLAG), any_vars(is.na(.))",
                                           status = "FINAL",
                                           definition = "concept mapped to a single cui that fully encapsulates meaning of the native concept",
                                           parent = "MAPPED_TO_SINGLE_CUI"),
                                data.frame(bifurcation = "1",
                                           sequence = "2",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(CUI_01), any_vars(!is.na(.))",
                                           status = "MAPPED_TO_COORDINATE_CUIS",
                                           definition = "concept mapped to a multiple cuis that varies in coverage and overlap of meaning of the native concept",
                                           parent = "PARENT"),
                                data.frame(bifurcation = "1",
                                           sequence = "3",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(CUI, CUI_01), all_vars(is.na(.))",
                                           status = "UNMAPPED",
                                           definition = "concepts that have not been mapped",
                                           parent = "PARENT"),
                                data.frame(bifurcation = "2",
                                           sequence = "1",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(contains('DENOVO')), any_vars(!(is.na(.)))",
                                           status = "DENOVO",
                                           definition = "concepts that may need denovo concept creation due to their meaning",
                                           parent = "UNMAPPED"),
                                data.frame(bifurcation = "2",
                                           sequence = "2",
                                           dplyr_function = "filter_at",
                                           dplyr_function_arg = "vars(contains('FOLLOWUP')), any_vars(!(is.na(.)))",
                                           status = "FOLLOWUP",
                                           definition = "concepts that may need denovo concept creation due to their meaning",
                                           parent = "UNMAPPED"),
                                data.frame(bifurcation = "1",
                                           sequence = "4",
                                           dplyr_function = "setdiff",
                                           dplyr_function_arg = "PARENT, dplyr::bind_rows(MAPPED_TO_SINGLE_CUI, MAPPED_TO_COORDINATE_CUIS, UNMAPPED)",
                                           status = "ORPHAN",
                                           definition = "concepts lost in this bifurcation",
                                           parent = "PARENT"),
                                data.frame(bifurcation = "2",
                                           sequence = "3",
                                           dplyr_function = "setdiff",
                                           dplyr_function_arg = "MAPPED_TO_SINGLE_CUI, dplyr::bind_rows(FLAGGED, FINAL)",
                                           status = "ORPHAN",
                                           definition = "concepts lost in this bifurcation",
                                           parent = "MAPPED_TO_SINGLE_CUI"),
                                data.frame(bifurcation = "2",
                                           sequence = "3",
                                           dplyr_function = "setdiff",
                                           dplyr_function_arg = "UNMAPPED, dplyr::bind_rows(DENOVO, FOLLOWUP)",
                                           status = "ORPHAN",
                                           definition = "concepts lost in this bifurcation",
                                           parent = "UNMAPPED")
                        ) %>%
                        dplyr::mutate_at(vars(bifurcation, sequence), as.integer) %>%
                        dplyr::arrange(bifurcation, parent, sequence)
                
                PARENT <- dataframe
                
                bifurcation_01 <-
                        cartograph %>%
                        dplyr::filter(bifurcation == 1)
                
                output_01 <- list()
                for (i in 1:nrow(bifurcation_01)) {
                        x <- 
                                eval(
                                        rlang::parse_expr(
                                                paste0(bifurcation_01$parent[i], " %>% ",
                                                       paste0("dplyr::", bifurcation_01$dplyr_function[i], "(", bifurcation_01$dplyr_function_arg[i], ")")))
                                )
                        
                        assign(bifurcation_01$status[i], x)
                        output_01[[i]] <- x
                        names(output_01)[i] <- bifurcation_01$status[i]
                }
                
                
                bifurcation_02 <-
                        cartograph %>%
                        dplyr::filter(bifurcation == 2)
                
                bifurcation_02 <-
                        split(bifurcation_02, bifurcation_02$parent)
                
                output_02 <- list()
                for (i in 1:length(bifurcation_02)) {
                        if (i == 1) {
                                output_02 <- list()
                                output_02[[1]] <- list()
                                names(output_02)[1] <- names(bifurcation_02)[1]
                        } else {
                                output_02[[i]] <- list()
                                names(output_02)[i] <- names(bifurcation_02)[i]
                        }
                }
                
                for (i in 1:length(bifurcation_02)) {
                        for (j in 1:nrow(bifurcation_02[[i]])) {
                                x <- 
                                        eval(
                                                rlang::parse_expr(
                                                        paste0(
                                                                names(bifurcation_02)[i], " %>% ",
                                                                paste0("dplyr::", bifurcation_02[[i]]$dplyr_function[j], "(", bifurcation_02[[i]]$dplyr_function_arg[j], ")"))
                                                )
                                        )
                                
                                output_02[[names(bifurcation_02)[i]]][[1+length(output_02[[names(bifurcation_02)[i]]])]] <- x
                                names(output_02[[names(bifurcation_02)[i]]])[length(output_02[[names(bifurcation_02)[i]]])] <- bifurcation_02[[i]]$status[j]
                        }
                }
                
                single_bifurcations <- bifurcation_01$status[!(bifurcation_01$status %in% names(output_02))]
                
                final_output <- list()
                for (i in 1:length(single_bifurcations)) {
                        final_output[[i]] <- output_01[[single_bifurcations[i]]]
                        names(final_output)[i] <- single_bifurcations[i]
                }
                final_output <- c(final_output, output_02)
                return(final_output)
        }