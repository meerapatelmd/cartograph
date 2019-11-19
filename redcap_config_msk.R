
##Local MSK repositories that support project
path_to_bibliotech_repo <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech"
path_to_mskextractquarry_repo <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry"

##Path to cartographR components within biblio-tech repository
path_to_identity <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/IDENTITY.csv"
path_to_key <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/KEY_TO_UMLS.csv"
path_to_relationship <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/RELATIONSHIP.csv"
path_to_umls_lookup <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/UMLS/LOOKUP.csv"
path_to_local_dictionary <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech//CATALOGUE/MSK/LOCAL_DICTIONARY.csv"

##Path to source data in msk-extract-quarry
path_to_endocrine <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/Data Dictionary Inventory from John Philip (2019-10-23)/LOADED/161228NeuroendocrineTumors_DataDictionary_2019-10-22.csv"
path_to_neuronc <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/Data Dictionary Inventory from John Philip (2019-10-23)/LOADED/ClinicalRadiologicalAndMolecul_DataDictionary_2019-10-22.csv"
path_to_pancreas <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/Data Dictionary Inventory from John Philip (2019-10-23)/LOADED/12245PancreasIMPACTDatabaseV20_DataDictionary_2019-10-23.csv"


endocrine <- readr::read_csv(path_to_endocrine, col_types = cols(.default = "c"))
endocrine_parsed <- format_source_redcap_data_dictionary(path_to_endocrine,
                                                        project_alias = "endocrine",
                                                        identity_id_starting_digit = "191023",
                                                        identity_id_prefix = "E") %>%
                        dplyr::mutate(VARIABLE_FIELD_NAME = stringr::str_replace_all(VARIABLE_FIELD_NAME, "^msk1301894$", "mrn")) %>%
                        dplyr::mutate(VARIABLE_FIELD_NAME = stringr::str_replace_all(VARIABLE_FIELD_NAME, "^msk0079399$", "gender"))

neuronc_parsed <- format_source_redcap_data_dictionary(path_to_neuronc,
                                                         project_alias = "neuronc",
                                                         identity_id_starting_digit = "191023",
                                                         identity_id_prefix = "N")

pancreas_parsed <- format_source_redcap_data_dictionary(path_to_pancreas,
                                                        project_alias = "pancreas",
                                                        identity_id_starting_digit = "191115",
                                                        identity_id_prefix = "P")

###
key <- load_and_filter_key_for_current_version(path_to_key = path_to_key, log = FALSE)


inventory <- dplyr::bind_rows(endocrine_parsed,
                      neuronc_parsed,
                      pancreas_parsed) %>%
                        somersaulteR::call_mr_clean()


inventory_joined_with_key <- list()
inventory_joined_with_key[[1]] <-
        dplyr::left_join(inventory %>%
                                 dplyr::filter(is.na(PERMISSIBLE_VALUE_LABEL)),
                         key %>%
                                 dplyr::select(-IDENTITY_ID),
                         by = c("VARIABLE_FIELD_NAME" = "KEY_CONCEPT_NAME"))

inventory_joined_with_key[[2]] <-
        dplyr::left_join(inventory %>%
                                 dplyr::filter(!is.na(PERMISSIBLE_VALUE_LABEL)),
                         key %>%
                                 dplyr::select(-IDENTITY_ID),
                         by = c("PERMISSIBLE_VALUE_LABEL" = "KEY_CONCEPT_NAME"))

names(inventory_joined_with_key) <- c("Variable", "Permissible_Values")




write_mrconso_net(inventory_joined_with_key$Variable[120:300,],
                  FIELD_LABEL,
                  path_to_lookup = path_to_umls_lookup,
                  path_to_umls_net_dir = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/UMLS/NETS",
                  push_to_github = FALSE)


lookup <- readr::read_csv(path_to_umls_lookup, col_types = cols(.default = "c"))
mapped.pv <-
        unmapped.pv %>%
        dplyr::mutate(PERMISSIBLE_VALUE_LABEL = stringr::str_replace_all(PERMISSIBLE_VALUE_LABEL, "[/]{1}|[_]{1}", "")) %>%
        inner_join(lookup, by = c("PERMISSIBLE_VALUE_LABEL" = "UMLS_SQL_KEYWORD")) %>%
        dplyr::filter(!is.na(COORDINATE_CUI)) %>%
        dplyr::select(-starts_with("NET"), -starts_with("UMLS_SQL_KEYWORD"), -starts_with("COORDINATE"))

unmapped.pv <-
        setdiff(unmapped.pv,
                mapped.pv)

unmapped.variable <-
        inventory %>%
        dplyr::select(PROJECT_ALIAS:KEY_WORD, ends_with("variable")) %>%
        dplyr::filter_at(vars(CUI.variable, STR.variable, PERMISSIBLE_VALUE_LABEL), all_vars((is.na(.)))) %>%
        dplyr::group_by(PROJECT_ALIAS, IDENTITY_ID, FORM_NAME, VARIABLE_FIELD_NAME, FIELD_LABEL, FIELD_TYPE, CHOICES_CALCULATIONS_OR_SLIDER_LABELS) %>%
        dplyr::filter(row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()

write_mrconso_net(unmapped.variable,
                  VARIABLE_FIELD_NAME,
                  path_to_lookup = path_to_umls_lookup,
                  path_to_umls_net_dir = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/UMLS/NETS")

View(
inventory.pv %>%
        dplyr::group_by(PROJECT_ALIAS, IDENTITY_ID, FORM_NAME, VARIABLE_FIELD_NAME, PERMISSIBLE_VALUE_LABEL, FIELD_LABEL, FIELD_TYPE) %>%
        dplyr::summarise(COUNT = length(PERMISSIBLE_VALUE_LABEL)) %>%
        dplyr::left_join(inventory.pv) %>%
        dplyr::filter(COUNT > 1)
)



        manifest <-
                inventory %>%
        somersaulteR::add_primary_key(pkey_column_name = "MANIFEST_ID") %>%
        dplyr::mutate(MANIFEST_ID = paste0("MANIFEST", stringr::str_pad(MANIFEST_ID, width = 5, side = "left", pad = "0"))) %>%
        dplyr::select(MANIFEST_ID, PROJECT_ALIAS, FORM_NAME, VARIABLE_FIELD_NAME, FIELD_LABEL, FIELD_TYPE, PERMISSIBLE_VALUE_LABEL, starts_with("CHOICES"), PERMISSIBLE_VALUE_LITERAL, PERMISSIBLE_VALUE_CODE)

rm(endocrine, endocrine_parsed, radiology_parsed)


split_by_form <-
        split(manifest, manifest$FORM_NAME)

names(split_by_form) <- paste0("FORM_NAME:", names(split_by_form))

metrics <-
manifest %>%
        dplyr::group_by(FORM_NAME) %>%
        dplyr::summarise(TOTAL_COUNT = n()) %>%
        dplyr::left_join(manifest %>%
                                 dplyr::filter(is.na(PERMISSIBLE_VALUE_LABEL)) %>%
                                 dplyr::group_by(FORM_NAME) %>%
                                 dplyr::summarise(VARIABLE_COUNT = length(VARIABLE_FIELD_NAME))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(PERMISSIBLE_VALUE_COUNT = TOTAL_COUNT - VARIABLE_COUNT)

metrics_schedule <- metrics
metrics_schedule_gp1 <-
        somersaulteR::filter_out_vector(metrics_schedule, FORM_NAME, group1)

group1 <- c("baseline", "demographics", "clinical_trial", "disease_stage_and_location", "status", "disease_stage_and_location")

136+2-+13+14+8+9+14

group2 <- c("brain_tumor_surgery", "glioma_score", "histologymolecular_sequencing", "longitudinal_mri", "pathology_and_samples", "surgery")
metrics_schedule_gp2 <-
        somersaulteR::filter_out_vector(metrics_schedule_gp1, FORM_NAME, group2)
19+154+40+98+3

group3 <- c("chemotherapy", "prior_therapies", "radiation_therapy", "radiology", "treatment")
#prior_therapies, chemotherapy, treatment
groups <- list(group1, group2, group3)
total_count_by_group <- vector()
for (i in 1:length(groups)) {
        total_count_by_group[i] <-
        metrics %>%
                somersaulteR::filter_for_vector(FORM_NAME, groups[[i]]) %>%
                summarise(sum(TOTAL_COUNT)) %>%
                unlist() %>%
                unname()
}

stage_label <- c("Ramp Up", "Peak", "Ramp Down")
deadlines <- c("2019-11-22", "2019-11-29 & 2019-12-06", "2019-12-13")
group_strings <- sapply(groups, paste, collapse = ", ")
total_count_by_group

schedule <-
        data.frame(PART = LETTERS[1:3],
                   STAGE = stage_label,
                   DEADLINE = deadlines,
                   FORM_NAMES = group_strings,
                   TOTAL_COUNT = total_count_by_group
        )

manifest_to_odysseus <-
        list(manifest)


relationship <- readr::read_csv(path_to_relationship, col_types = cols(.default = "c"))
mirCat::clipboard_copy(relationship)
#table_of_contents <- list()
table_of_contents_fns <- c(
"/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Architecture/TABLE_OF_CONTENTS_DENOVO_BUCKET.csv",
"/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Architecture/TABLE_OF_CONTENTS_KEY.csv",
"/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Architecture/TABLE_OF_CONTENTS_REPORT.csv")
table_of_contents <- lapply(table_of_contents_fns, readr::read_csv, col_types = cols(.default = "c"))

final_table_of_contents <- list()
final_table_of_contents <- table_of_contents[[3]][-1,] %>%
                                        dplyr::select(TAB_NAME, TAB_CONTENTS)

FINAL <- list(final_table_of_contents,
              manifest,
              schedule,
              metrics)

openxlsx::write.xlsx(FINAL, "CONCEPT_MAPPING_INVENTORY_2019-11-14.xlsx")

#final_table_of_contents <- rbind(final_table_of_contents, c("SCHEDULE", "dates determining cadence of concept mapping"))
#final_table_of_contents <- rbind(final_table_of_contents, c("MANIFEST", "complete concept payload"))
#final_table_of_contents <- rbind(final_table_of_contents, c("INVENTORY", "list of concepts to be mapped"))

# IDENTITY <- readr::read_csv(path_to_identity, col_types = cols(.default = "c"))
# IDENTITY_LONG <- cartographR::identity_from_csv(path_to_identity = path_to_identity, target_cols = c("VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL"))
#
# IDENTITY_LONG %>%
#         dplyr::filter_at(vars(KEY_CONCEPT_NAME), any_vars(grepl("Anti-HER2 Therapy - Trastuzumab-Based", ., ignore.case = TRUE) == TRUE))
#
#
# retrieve_key <-
#         function(path_to_key, log_source_comment = "") {
#                 key <- mirCat::my_read_csv(path_to_key, log_source_comment = log_source_comment)
#         }
