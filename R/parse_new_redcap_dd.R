#' Parses a new redcap data dictionary
#' @export

parse_new_redcap_dd <-
        function(path_to_redcap_dd,
                 project_alias,
                 identity_id_starting_digit,
                 identity_id_prefix) {
                return(format_source_redcap_data_dictionary(path_to_redcap_data_dictionary_csv = path_to_csv,
                                                                   project_alias = project_alias,
                                                                   identity_id_starting_digit = "1000",
                                                                   identity_id_prefix = project_alias)
                )
        }