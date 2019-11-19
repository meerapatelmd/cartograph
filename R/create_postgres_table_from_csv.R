
athena_table_fns <- list.files("/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CATALOGUE/Athena_Vocabulary_v5", pattern = "csv$", full.names = TRUE)
tablenames <- tolower(mirroR::strip_fn(athena_table_fns))



conn <- connect_to_local_postgres(dbname = "athena")
output <- list()
for (i in 1:length(tablenames)) {
        tablename <- tablenames[i]
        sql_statement <- paste0("SELECT DISTINCT COUNT(*) FROM public.", tablename, ";")
        output[[i]] <- DBI::dbGetQuery(conn, sql_statement)
}
DBI::dbDisconnect(conn)




fn <- athena_table_fns[3]
dataframe <- readr::read_delim(fn, n_max = 1, delim = "\t", col_types = cols(.default = "c"))
character_count <- nchar(dataframe$concept_synonym_name)

test <-
dataframe %>%
        dplyr::mutate(character_count = nchar(concept_synonym_name)) %>%
        dplyr::filter(is.na(character_count))

rm(dataframe)


for (i in 1:ncol(dataframe)) {
        max(nchar(dataframe[,i]))
}


write_create_table_sql_from_csv <-
        function(path_to_csv, delim = "\t", output_sql_fn = "source_tables.sql") {

                table_name <- tolower(mirroR::strip_fn(path_to_csv))
                field_names <- readr::read_delim(path_to_csv, delim = delim, col_names = FALSE, n_max = 1) %>%
                                                unlist() %>%
                                                unname()

                sql_stem <- paste("DROP TABLE IF EXISTS", paste0(table_name, ";"), "\n",
                                    "\n",
                                    "CREATE TABLE", table_name, "\n",
                                    "(\n")

                sql_stem_w_fields <- sql_stem

                for (i in 1:length(field_names)) {
                        if (i != length(field_names)) {
                                field_name <- field_names[i]
                                sql_stem_w_fields <-
                                        c(sql_stem_w_fields,
                                          paste("\t", field_name, "varchar(255),\n", collapse = " "))
                        } else if (i == length(field_names)) {
                                field_name <- field_names[i]
                                sql_stem_w_fields <-
                                        c(sql_stem_w_fields,
                                          paste("\t", field_name, "varchar(255)\n)\n;\n\n", collapse = " "))
                        }

                }

                if (!(file.exists(output_sql_fn))) {
                        cat(sql_stem_w_fields, file = output_sql_fn)
                } else {
                        answer <- readline("File exists. [O]verwrite/[a]ppend? ")
                        if (answer == "O") {
                                file.remove(output_sql_fn)
                                cat(sql_stem_w_fields, file = output_sql_fn)
                        } else if (answer == "a") {
                                cat(sql_stem_w_fields, file = output_sql_fn, append = TRUE)
                        } else {
                                typewriteR::tell_me("Invalid option. Please try again.")
                        }
                }


        }
