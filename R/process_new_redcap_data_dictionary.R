#' Process new redcap data dictionary into the parsed, IDENTITY format and added to the IDENTITY component and biblio-tech repo is commited
#' @param project_alias character string of length 1 for "PROJECT_ALIAS"
#' @param identity_id_starting_digit character string of length 1 for the number the primary key should start at
#' @param identity_id_prefix character string of length 1 for the prefix each primary key should have
#' @param commit_message commit message when new changes are pushed the biblio-tech origin master
#' @import mirCat
#' @export

process_new_redcap_data_dictionary <-
        function(path_to_redcap_data_dictionary_csv,
                 project_alias,
                 identity_id_starting_digit,
                 identity_id_prefix,
                 commit_message) {

                output <-
                        format_source_redcap_data_dictionary(path_to_redcap_data_dictionary_csv = path_to_redcap_data_dictionary_csv,
                                                             project_alias = project_alias,
                                                             identity_id_starting_digit = identity_id_starting_digit,
                                                             identity_id_prefix = identity_id_prefix)

                mirCat::append_csv(path_to_identity,
                                      dataframe = output)

                mirCat::git_status(path_to_bibliotech_repo)
                mirCat::git_add_all(path_to_bibliotech_repo)
                mirCat::git_commit(path_to_bibliotech_repo, commit_message = commit_message)
                mirCat::git_push_to_msk(path_to_bibliotech_repo)
        }
