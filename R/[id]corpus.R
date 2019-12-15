#' Corpus Functions
#' @import mirCat
#' @importFrom projektoR append_csv
#' @importFrom mirroR create_path_to_file
#' @export

add_terms_to_corpus <-
        function(corpus_name, new_terms, path_to_corpus = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CORPUS", local_github_repo = NULL) {
                fn <- mirroR::create_path_to_file(path_folder = path_to_corpus,
                                            basename = corpus_name,
                                            file_extension = "csv")
                projektoR::append_csv(csv_fn = fn,
                                      dataframe = data.frame(CORPUS = new_terms))

                if (!is.null(local_github_repo)) {
                        mirCat::git_add_all(local_github_repo)
                        mirCat::git_commit(local_github_repo)
                        mirCat::git_push_to_msk(local_github_repo)
                }
        }

#' Corpus Functions
#' @import mirCat
#' @importFrom projektoR append_csv
#' @importFrom mirroR create_path_to_file
#' @export


load_corpus <-
        function(path_to_corpus = "/Users/meerapatel/GitHub/MSK_KMI_Enterprise/biblio-tech/CORPUS") {
                fns <- list.files(path_to_corpus)
                output <- lapply(fns, readr::read_csv, col_types = cols(.default = "c"))
                names(output) <- mirroR::strip_fn(fns)
                assign("CORPUS", output, envir = globalenv())
        }
