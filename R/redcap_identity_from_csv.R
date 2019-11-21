#' Gets IDENTITY data and cleans up based on parameters
#' @param path_to_identity path to the rawidentity csv
#' @param target_cols vector of the names of the columns that will be melted into the "KEY_FIELD" variable
#' @param keep_all_cols TRUE if all the variables are wanted in the output. Otherwise, only the target_cols will be selected.
#' @param include_duplicates TRUE if all the duplicates are desired in the output
#' @param dont_trimws TRUE if white spaces on both the left and right side are not desired in the output
#' @param remove_all character string of length 1 representing the string of artifact and other values to be removed from the final output separated by the OR operator
#' @import mirCat
#' @import somersaulteR
#' @importFrom reshape2 melt
#' @import dplyr
#' @import readr
#' @return dataframe where raw REDCap data dictionary dataframe is processed to a concept level where the target_cols parameter identifies the target columns in the raw data as concepts
#' @export

redcap_identity_from_csv <-
        function(path_to_identity,
                 target_cols,
                 redcap_forms_to_exclude = NULL,
                 rm_na_concepts = TRUE,
                 keep_all_cols = TRUE,
                 dont_trimws = FALSE,
                 remove_all = "\t|\n",
                 include_duplicates = FALSE,
                 exclude_concepts = NULL,
                 log = TRUE) {

                IDENTITY_TARGET_COLS <- c("IDENTITY_ID", target_cols)
                IDENTITY_00 <- readr::read_csv(path_to_identity, col_types = readr::cols(.default = "c"))

                if (!is.null(redcap_forms_to_exclude)) {
                        IDENTITY_01 <-
                                IDENTITY_00 %>%
                                somersaulteR::filter_out_vector(filter_col = FORM_NAME,
                                                                exclusion_vector = redcap_forms_to_exclude)
                } else {
                        IDENTITY_01 <-
                                IDENTITY_00
                }

                if (keep_all_cols == TRUE) {
                        IDENTITY_02 <- IDENTITY_01
                } else {
                        IDENTITY_02 <-
                                IDENTITY_01 %>%
                                dplyr::select(IDENTITY_TARGET_COLS)
                }

                if (dont_trimws == TRUE) {
                        IDENTITY_03 <- IDENTITY_02
                } else {
                        IDENTITY_03 <-
                                IDENTITY_02 %>%
                                dplyr::mutate_all(trimws, "both")
                }

                if (remove_all == ""|is.null(remove_all)) {
                        IDENTITY_04 <-
                                IDENTITY_03
                } else {
                        IDENTITY_04 <-
                                IDENTITY_03 %>%
                                dplyr::mutate_all(stringr::str_remove_all, remove_all)
                }



                IDENTITY_05 <-
                        reshape2::melt(IDENTITY_04, measure.vars = c("VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL"), variable.name = "KEY_FIELD", value.name = "KEY_CONCEPT_NAME") %>%
                        dplyr::mutate_all(as.character)

                if (include_duplicates == TRUE) {
                        IDENTITY_06 <- IDENTITY_05
                } else {
                        IDENTITY_06 <-
                                IDENTITY_05 %>%
                                dplyr::distinct()
                }

                if (rm_na_concepts == TRUE) {
                        IDENTITY_07 <-
                                IDENTITY_06 %>%
                                dplyr::filter(!is.na(KEY_CONCEPT_NAME))
                } else {
                        IDENTITY_07 <-
                                IDENTITY_06
                }

                if (!is.null(exclude_concepts)) {
                        IDENTITY_08 <-
                                IDENTITY_07 %>%
                                somersaulteR::filter_out_vector(filter_col = KEY_CONCEPT_NAME,
                                                                exclusion_vector = exclude_concepts)
                } else {
                        IDENTITY_08 <-
                                IDENTITY_07
                }

                IDENTITY_NATIVE_FORMAT <- readr::read_csv(path_to_identity, col_types = cols(.default = "c"))

                FINAL_IDENTITY <-
                        dplyr::inner_join(IDENTITY_NATIVE_FORMAT, IDENTITY_08)


                if (log == TRUE) {
                        mirCat::log_this_as(project_log_dir = dirname(path_to_identity),
                                            project_log_load_comment = "identity",
                                            project_log_fn = basename(path_to_identity),
                                            project_log_fn_md5sum = tools::md5sum(basename(path_to_identity)),
                                            project_log_load_timestamp = mirroR::get_timestamp()
                                            )

                        return(FINAL_IDENTITY)
                } else {
                        return(FINAL_IDENTITY)
                }
        }
