
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
path_to_input_01 <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/Misc_20190819_to_20191023/BreastDMT_DataDictionary_2018-07-27.csv"
path_to_input_02 <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/One_Offs/Missing Mappings.xlsx"



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
