#' Printing A Witness_Set Object
#'
#' @param x an object of class witness_set
#' @usage \method{print}{witness_set}(x)
#' @return Invisible string of the printed object.
#'
#' @export
#'
#' @examples
#'
#' # run initial numerical irreducible decomposition for
#' # the equation xy = 0
#'
#' input <- bertini_input("x*y")
#' input <- modify_config(input, tracktype = 1)
#'
#' out <- bertini(input, output = "pos_dim")
#'
#' # print the witness set for all components in dimension 1
#' witness_set <- witness_set_print(out, 1, -2)
#' print(witness_set)
print.witness_set <- function(x) {

  stopifnot(is.witness_set(x))

  points <- x$points[-c(1,2)]

  cat("Witness Points: \n")
  cat(glue_collapse(points, sep = "\n"))
  cat("\n")

  linear_system <- x$system[-c(1:3)]
  linear_system <- str_replace_all(linear_system, "\\*I", "i")
  linear_system <- str_replace(linear_system, "END;", "")

  cat("Linear Systems:")
  cat(glue_collapse(linear_system, sep = "\n"))
}

