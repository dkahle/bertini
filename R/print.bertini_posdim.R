#' Printing Positive-Dimensional Bertini Solution Sets
#'
#' For positive-dimensional runs in Bertini, the most interesting
#' output comes from the numerical irreducible decompositions. This
#' printing method displays the most interesting information from the
#' output to the screen. The two tables are the Witness Set Decomposition
#' and the Decomposition by Degree.
#'
#' @param x an object of class bertini_posdim
#' @param ... additional parameters
#' @return Invisible string of the printed object.
#' @usage \method{print}{bertini_posdim}(x, ...)
#' @export
#'
#' @examples
#'
#' if (has_bertini()) {
#'
#' # see ?bertini
#'
#' # solution set to xy = 0
#'
#' # using bertini_input syntax
#' input <- bertini_input("x*y")
#' input <- modify_config(input, tracktype = 1)
#'
#' # solution set: x and y axis in complex space (2 connected components)
#' bertini(input, output = "pos_dim")
#'
#'
#'
#'
#' # solution set to xz + y = 0, yz + x = 0
#'
#' input <- bertini_input(c("x*z + y", "y*z + x"))
#' input <- modify_config(input, tracktype = 1)
#'
#' # solution set: 3 lines (x=y=0 then z is free,
#' #                        x=y!=0 then z=-1,
#' #                        x=-y!=0 then z=1)
#'
#' bertini(input, output = "pos_dim")
#' }



print.bertini_posdim <- function(x, ...) {

  stopifnot(is.bertini_posdim(x))

  index <- which(str_detect(x$bertini_out, "Witness Set Decomposition"))

  # subset so only interesting stuff remains
  interesting_output <- x$bertini_out[index:length(x$bertini_out)]

  cat(glue_collapse(interesting_output, sep = "\n"))
}
