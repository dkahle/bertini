#' Bertini_Input Object Check
#'
#' Test whether an object is an bertini_input object.
#'
#' @param x object to be tested
#' @return Vector of logicals.
#' @export
#' @examples
#'
#' if (has_bertini()) {
#'
#' polys <- mp(c("x^2 + y^2 - 1", "x - y"))
#' struct <- bertini_input(polys)
#' is.bertini_input(struct)
#'
#' }
#'

is.bertini_input <- function(x) inherits(x, "bertini_input")
