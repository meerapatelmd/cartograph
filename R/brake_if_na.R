#' Brakes if a value is NA
#' @param value character vector of length 1
#' @return warning message in the console with option to continue by pressing [ENTER]
#' @importFrom crayon red
#' @importFrom typewriteR tell_me
#' @importFrom typewriteR stop_and_enter
#' @export

brake_if_na <-
        function(value) {
                value_name <- deparse(substitute(value))
                if (is.na(value)) {
                        typewriteR::tell_me(crayon::red("Warning:", value_name, "is <NA>."))
                        typewriteR::stop_and_enter()
                }
        }
