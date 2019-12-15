#' Reads key
#' @param project_alias project_alias for the specific project in the key; "all" if all projects desired.
#' @import readr
#' @import dplyr
#' @export

read_key <-
        function(path_to_key,
                 project_alias,
                 log = TRUE) {
                
                if (project_alias != "all") {
                        if (log == TRUE) {
                                mirCat::log_this_as(project_log_dir = dirname(path_to_key),
                                                    project_log_load_comment = "read_key",
                                                    project_log_fn = basename(path_to_key),
                                                    project_log_fn_md5sum = tools::md5sum(basename(path_to_key)),
                                                    project_log_load_timestamp = mirroR::get_timestamp()
                                                    #project_log_gsheet_title = gsheet_name,
                                                    #project_log_gsheet_tab_name = gsheet_tab,
                                                    #project_log_gsheet_key = mirCat::gs_id_from_name(gsheet_name)
                                )
                        }
                        return(readr::read_csv(path_to_key, col_types = readr::cols(.default = "c")) %>% 
                                       dplyr::filter(PROJECT_ALIAS == project_alias))
                } else {
                        if (log == TRUE) {
                                mirCat::log_this_as(project_log_dir = dirname(path_to_key),
                                                    project_log_load_comment = "read_key",
                                                    project_log_fn = basename(path_to_key),
                                                    project_log_fn_md5sum = tools::md5sum(basename(path_to_key)),
                                                    project_log_load_timestamp = mirroR::get_timestamp()
                                                    #project_log_gsheet_title = gsheet_name,
                                                    #project_log_gsheet_tab_name = gsheet_tab,
                                                    #project_log_gsheet_key = mirCat::gs_id_from_name(gsheet_name)
                                )
                        }
                        return(readr::read_csv(path_to_key, col_types = readr::cols(.default = "c")))
                }
                
                
        }
