library(tidyverse)

##Local MSK repositories that support project
path_to_bibliotech_repo <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech"
path_to_mskextractquarry_repo <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry"

##Path to cartographR components within biblio-tech repository
path_to_identity <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/IDENTITY.csv"
path_to_key <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/KEY_TO_UMLS.csv"
path_to_relationship <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/KEY/REDCap/RELATIONSHIP.csv"
path_to_umls_lookup <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/UMLS/LOOKUP.csv"
path_to_athena_lookup <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Athena_Vocabulary_v5/LOOKUP.csv"

##Path to source data in msk-extract-quarry
path_to_input_01 <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/Misc_20190819_to_20191023/BreastDMT_DataDictionary_2018-07-27.csv"
path_to_input_02 <- "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/msk-extract-quarry/One_Offs/Missing Mappings.xlsx"


umls_lookup <- readr::read_csv(path_to_umls_lookup, col_types = cols(.default = "c"))
write_athena_net(umls_lookup$UMLS_SQL_KEYWORD,
                 path_to_repo = path_to_bibliotech_repo)

key <- readr::read_csv(path_to_key, col_types = cols(.default = "c"))

write_athena_net(key$STR[!is.na(key$STR)],
                 path_to_repo = path_to_bibliotech_repo)
