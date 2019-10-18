#' User-Defined Homotopies
#'
#' A framework for user-defined homotopies in bertini. Due to the highly variable nature
#' of the input for user-defined homotopies, most of the front-end work is done by the user.
#' See examples below for proper syntax.
#' @param polys system of polynomials as either a character vector, mpoly, or mpolyList
#' @param hom_type type of user-defined homotopy to be run; either affine or projective
#' @param configurations a optional named list of configurations to be given to
#' Bertini. Names will be changed to lower case. The function will
#' automatically assign a value for the \code{userhomotopy} configuration.
#' @param definitions definitions block of the input file. For a user-defined homotopy,
#' a path-variable and at least one parameter must be defined.
#' @param subfunctions a named list that contains subfunctions, path variable assignment,
#'  and other equations needed in the input file but do not need to be named in the
#' definitions block.
#' @param start_points starting points that are solutions to the start system. They need
#' to be provided as a list of vectors.
#'
#' @return an object of class bertini
#' @export
#'
#' @examples
#'
#' if(has_bertini()) {
#'
#'
#'
#' ## Affine example from Bates et.al 2013, pg. 36
#'
#' poly <- mp("z^2 - s")
#' definitions <- list("variable" = "z",
#'                     "function" = "H",
#'                     "parameter" = c("q1", "q2"),
#'                     "pathvariable" = "t")
#' subfunctions <- list("q1" = "cos(2*Pi*(1-t))",
#'                      "q2" = "sin(2*Pi*(1-t))",
#'                      "s" = "q1 + I*q2")
#' start_points <- list(c(1,0))
#'
#' user_defined_homotopy(poly, hom_type = "affine",
#'                       definitions = definitions,
#'                       subfunctions = subfunctions,
#'                       start_points = start_points)
#'
#'
#'
#' ## Projective space example from the Bertini users manual
#'
#' poly <- mp("(x^2 - z^2)*gamma*s + (x^2)*(1-s)")
#' definitions <- list("hom_variable_group" = c("x", "z"),
#'                     "function" = "f",
#'                     "pathvariable" = "t",
#'                     "parameter" = "s",
#'                     "constant" = "gamma")
#' subfunctions <- list("gamma" = "0.8 - 1.2*I",
#'                 "s" = "t")
#' start_points <- list(c(1,1), c(1,-1))
#'
#' user_defined_homotopy(poly, hom_type = "projective",
#'                       definitions = definitions,
#'                       subfunctions = subfunctions,
#'                       start_points = start_points)
#'
#'
#' }
user_defined_homotopy <- function(polys,
                                  hom_type = c("affine", "projective"),
                                  configurations = list(),
                                  definitions,
                                  subfunctions,
                                  start_points) {
  # match arguments
  hom_type <- match.arg(hom_type)

  # make bertini input object
  input <- bertini_input(polys = polys,
                         configurations = configurations,
                         definitions = definitions,
                         subfunctions = subfunctions
                         )

  # make sure that configuration matches function input
  if(hom_type == "affine") {
    input <- modify_config(input, userhomotopy = 1)
  } else if(hom_type == "projective") {
    input <- modify_config(input, userhomotopy = 2)
  }

  # call bertini
  out <- bertini(input, start_points = start_points)

  out
}
