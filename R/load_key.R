#' Gets KEY data and cleans up based on parameters
#' @param path_to_key full file path to key csv file
#' @param log TRUE if this event will be logged using the log functions from the mirCat package
#' @param project_alias since the KEY has all elements of all projects, project_alias is used to subset for the project
#' @return dataframe read from path_to_key that is grouped by the IDENTITY_ID, KEY_FIELD, and KEY_CONCEPT_NAME and filtered for the most recent record
#' @import mirCat
#' @import stringr
#' @import rubix
#' @import dplyr
#' @import readr
#' @importFrom lubridate ymd_hms
#' @export

load_key <-
        function(path_to_key,
                 log = TRUE,
                 project_alias) {
                
                key <- list()
                
                if (project_alias != "all") {
                        key[[1]] <- readr::read_csv(path_to_key, col_types = readr::cols(.default = "c")) %>%
                                dplyr::filter(PROJECT_ALIAS == project_alias)
                } else {
                        key[[1]] <- readr::read_csv(path_to_key, col_types = readr::cols(.default = "c"))
                }

                #Timestamp brake
                mirCat::brake_for_timestamp_format(key[[1]]$KEY_TIMESTAMP)

                #Here we go
                key[[2]] <-
                        key[[1]] %>%
                        dplyr::mutate(KEY_TIMESTAMP = lubridate::ymd_hms(KEY_TIMESTAMP)) %>%
                        dplyr::mutate(KEY_FIELD = stringr::str_replace_all(KEY_FIELD, "^KMI_PERMISSIBLE_VALUE_LABEL$", "PERMISSIBLE_VALUE_LABEL")) %>%
                        dplyr::group_by(IDENTITY_ID, KEY_FIELD, KEY_CONCEPT_NAME) %>%
                        dplyr::arrange(desc(KEY_TIMESTAMP)) %>%
                        dplyr::filter(KEY_TIMESTAMP == max(KEY_TIMESTAMP)) %>%
                        dplyr::ungroup() %>%
                        rubix::call_mr_clean() %>%
                        dplyr::select(KEY_TIMESTAMP, IDENTITY_ID, everything())
                
                names(key) <- c("HISTORY", "CURRENT")

                if (log == TRUE) {
                        mirCat::log_this_as(project_log_dir = dirname(path_to_key),
                                            project_log_load_comment = "load_key",
                                            project_log_fn = basename(path_to_key),
                                            project_log_fn_md5sum = tools::md5sum(basename(path_to_key)),
                                            project_log_load_timestamp = mirroR::get_timestamp()
                                            #project_log_gsheet_title = gsheet_name,
                                            #project_log_gsheet_tab_name = gsheet_tab,
                                            #project_log_gsheet_key = mirCat::gs_id_from_name(gsheet_name)
                        )
                }
                
                output <- list()
                output[[1]] <- key[[2]]
                output[[2]] <- key[[1]]
                names(output) <- c("CURRENT", "HISTORY")

                return(output)

        }
