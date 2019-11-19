source("config.R")
paths_to_local_repos <- sapply(objects(pattern = "path_to_.*repo$"), get, USE.NAMES = TRUE)
if (length(paths_to_local_repos)) {
        typewriteR::tell_me("Getting git status of supporting local repositories:")
        for (i in 1:length(paths_to_local_repos)) {
                typewriteR::tell_me("\t", names(paths_to_local_repos)[i])
        }
        cat("\n")

        for (i in 1:length(paths_to_local_repos)) {
                typewriteR::tell_me(basename(paths_to_local_repos[i]))
                mirCat::git_status(paths_to_local_repos[i])
                cat("\n\n")
        }
}

