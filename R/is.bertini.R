#' Bertini Object Check
#'
#' Test whether an object is an bertini object.
#'
#' @param x object to be tested
#' @return Vector of logicals.
#' @export
#' @examples
#'
#' if (has_bertini()) {
#'
#' soln <- poly_solve("x^2 - 1 == 0")
#' is.bertini(soln)
#'
#' }
#'
is.bertini <- function(x) inherits(x, "bertini")

