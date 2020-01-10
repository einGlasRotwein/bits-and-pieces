
# Functions for printing decimals taken from the package prmisc by Martin Papenberg:
# https://github.com/m-Py/prmisc

#' Force printing a specified number of decimals for a number
#'
#' @param x the numeric values to be printed
#' @param decimals how many decimals are to be printed. Defaults to 2.
#'
#' @return The number in the required format
#'
#' @examples
#' 
#' force_decimals(c(1.23456, 0.873, 2.3456))
#' force_decimals(c(1.23456, 0.873, 2.3456), 3)
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#' @export
#'
force_decimals <- function(x, decimals = 2) {
  return(vectorize_print(x, decimals, force_decimals_))
}

force_decimals_ <- function(x, decimals) {
  if (is.na(x)) {
    return(NA_character_)
  }
  return(format(round(x, decimals), nsmall = decimals, scientific = FALSE))
}

## An abstract function used to vectorize all number printing functions
vectorize_print <- function(x, decimals, FUN, ...) {
  x <- as.numeric(x)
  x_ <- vapply(x, FUN, FUN.VALUE = "character", decimals, ...)
  return(x_)
}
