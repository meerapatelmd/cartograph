path_to_identity <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/BiblioTech/KEY/REDCap/IDENTITY.csv"
path_to_key <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/BiblioTech/KEY/REDCap/KEY_TO_UMLS.csv"
path_to_relationship <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/BiblioTech/KEY/REDCap/RELATIONSHIP.csv"
path_to_umls_lookup <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/BiblioTech/CATALOGUE/UMLS/LOOKUP.csv"


IDENTITY <- readr::read_csv(path_to_identity, col_types = cols(.default = "c"))
IDENTITY_LONG <- cartographR::identity_from_csv(path_to_identity = path_to_identity, target_cols = c("VARIABLE_FIELD_NAME", "PERMISSIBLE_VALUE_LABEL"))

IDENTITY_LONG %>%
        dplyr::filter_at(vars(KEY_CONCEPT_NAME), any_vars(grepl("Anti-HER2 Therapy - Trastuzumab-Based", ., ignore.case = TRUE) == TRUE))

KEY <- cartographR::key_from_csv(path_to_key = path_to_key)

KEY %>%
        dplyr::filter(KEY_CONCEPT_NAME == "Anti-HER2 therapy - Trastuzumab-Based")

KEY %>%
        dplyr::filter(KEY_CONCEPT_NAME == "First Metastatic Sites")

KEY %>%
        dplyr::filter(KEY_CONCEPT_NAME == "Mets Sites at Last Followup")

KEY %>%
        dplyr::filter(KEY_CONCEPT_NAME == "pN")


retrieve_key <-
        function(path_to_key, log_source_comment = "") {
                key <- mirCat::my_read_csv(path_to_key, log_load_comment = "retrieve cartographR key", log_source_comment = log_source_comment)
        }
