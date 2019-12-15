#' Gets all the full file paths in biblio-tech for function calls
#' @param parts which parts of the biblio-tech that will be included in the object returned
#' @return list of length of `parts`, non-recursive file list and dir list
#' @importFrom purrr transpose
#' @export

find_bibliotech_paths <-
        function(path_to_bibliotech_repo, parts = c("CATALOGUE", "KEY")) {
                orchard <- list()
                for (i in 1:length(parts)) {
                        part <- parts[i]
                        path_to_bibliotech_part <- paste0(path_to_bibliotech_repo, "/", part)

                        dirs     <- list.dirs(path_to_bibliotech_part, recursive = FALSE)

                        ##Obtaining Catalogue Tree
                        tree <- list()
                        files <- sapply(dirs, list.files, simplify = FALSE, full.names = TRUE)
                        tree[[1]] <- sapply(1:length(files), function(i) grep("^.*[.]{1}.*$",
                                                                              files[[i]],
                                                                              value = TRUE),
                                            simplify = FALSE
                        )

                        tree[[2]]  <- sapply(1:length(files), function(i) grep("^.*[.]{1}.*$",
                                                                               files[[i]],
                                                                               value = TRUE,
                                                                               invert = TRUE),
                                             simplify = FALSE
                        )

                        names(tree) <- c("FILES", "DIRS")
                        tree <- purrr::transpose(tree, .names = basename(names(files)))
                        orchard[[i]] <- tree
                        names(orchard)[i] <- part
                }
                return(orchard)
        }
