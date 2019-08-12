#' Bertini_Posdim Object Check
#'
#' Test whether an object is an bertini_posdim object.
#'
#' @param x object to be tested
#' @return Vector of logicals.
#' @export
#' @examples
#'
#' if (has_bertini()) {
#' input <- bertini_input("x*y")
#' input <- modify_config(input, tracktype = 1)
#'
#' out <- bertini(input, output = "pos_dim")
#' is.bertini_posdim(out)
#'
#' }
#'

is.bertini_posdim <- function(x) inherits(x, "bertini_posdim")
