#' Generate next ID number
#' @param id_vector vector of id numbers that is already existing. NA values are ignored.
#' @return character vector of length 1 of the next ID number by random addition of a value between 1 and 9
#' @import readr
#' @importFrom gmp as.bigz
#' @export

generate_next_id_number <-
        function(id_vector) {
                as.character(max(gmp::as.bigz(id_vector), na.rm = TRUE) + (sample(1:9, 1)))
        }