#' Brake if nrows don't match between dataframes
#' @param dataframe1 first dataframe
#' @param dataframe2 second dataframe
#' @import typewriteR
#' @export

brake_if_nrows_dont_match <-
        function(dataframe1, dataframe2) {
                if (nrow(dataframe1) != nrow(dataframe2)) {
                        typewriteR::tell_me("Warning: Number of rows do not match between dataframes.")
                        typewriteR::stop_and_enter()
                }
        }
