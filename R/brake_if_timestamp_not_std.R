#' Stops the script and asks the user to press enter if a timestamp isn't in standard format (ie 2019-11-21 08:00:00)
#' @param vector timestamp vector
#' @return interactive message
#' @importFrom typewriteR tell_me
#' @importFrom typewriteR stop_and_enter
#' @importFrom crayon red
#' @export

brake_if_timestamp_not_std <-
        function(vector) {
                if (all(grepl("^20[1,2]{1}[0-9]{1}[-]{1}[0-9]{2}[-]{1}[0-9]{2}[ ]{1}[0-9]{2}[:]{1}[0-9]{2}[:]{1}[0-9]{2}$", vector)) != TRUE) {
                        typewriteR::tell_me(crayon::red("Warning: timestamps not in standard format."))
                        typewriteR::stop_and_enter()
                }
        }
