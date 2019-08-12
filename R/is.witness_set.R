#' Witness_Set Object Check
#'
#' Test whether an object is an witness_set object.
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
#' # positive-dimensional run
#' out <- bertini(input, output = "pos_dim")
#'
#' # print witness set
#' witness_set <- witness_set_print(out, 1, -2)
#' is.witness_set(witness_set)
#'
#' }
#'

is.witness_set <- function(x) inherits(x, "witness_set")
