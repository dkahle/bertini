#' Create a Parameter Homotopy Bertini Input File
#'
#' Configure a Bertini input file to run a parameter homotopy
#'
#' @param mpolyList system of polynomials as either a character vector or mpolyList
#' @param parameters the parameters that define the family as a character vector
#' @param run_type the step of the parameter homotopy you are running. \code{offline}
#' instructs the function to solve for the general solutions, and \code{online} instructs
#' the function to solve for specific solutions based on data.
#' @param definitions an optional named list of the definitions to be given to
#'    Bertini. The definitions name all arguments used in the polynomial and
#'    tell Bertini what type of homotopy to use. Defaults to a total-degree homotopy.
#' @param configurations an optional named list of configurations to be given to
#'    Bertini. Names will be changed to lower case.
#' @param subfunctions an optional named list that contains subfunctions and
#' other equations needed in the input file but do not need to be named in the
#' definitions block.
#' @return an object of class bertini_input
#' @export
#'
#' @examples
#'
#'  polys <- c("-r11^2 + p00^2*p11*l1 + p00^2*l2",
#' "-r12^2 - p01^2*p10*l1 + p01^2*l2",
#' "-r21^2 - p10^2*p01*l1 + p10^2*l2",
#' "-r22^2 + p11^2*p00*l1 + p11^2*l2",
#' "p00*p11 - p01*p10",
#' "p00 + p01 + p10 + p11 - 1")
#'
#' parameter_homotopy(polys, c("r11","r12","r21","r22"))
#'


parameter_homotopy <- function(mpolyList,
                               parameters,
                               varorder,
                               run_type = c("offline", "online"),
                               definitions = list(),
                               configurations = list(),
                               subfunctions = list()) {

  # set up
  run_type <- match.arg(run_type)

  # feed to bertini_input
  struct <- bertini_input(mpolyList,
                          varorder,
                          definitions = definitions,
                          configurations = configurations,
                          subfunctions = subfunctions)

  # set parameter_homotopy configurations
  if(run_type == "offline") {
    struct <- modify_config(struct, parameterhomotopy = 1)
  } else {
    struct <- modify_config(struct, parameterhomotopy = 2)
  }

  # modify definitions

  # extract parameters
  struct$defs_block <- lapply(struct$defs_block, function(x) x[!x %in% parameters])

  # put in parameters line
  struct <- modify_defs(struct, parameter = parameters)

  # return structure
  struct
}
