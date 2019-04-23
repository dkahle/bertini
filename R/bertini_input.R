#' Base Bertini_Input Creation
#'
#' Create a basic bertini_input object from a system of polynomial equations.
#' A bertini_input object is a convenient way to store the information needed in
#' a Bertini input file. By default, \code{bertini_input} will create an
#' object that will be parsed as a total-degree homotopy in Bertini by grouping
#' all variables present, either in \code{varorder} or infered from
#' \code{mpolyList}, together in one variable group.
#'
#'
#' @param mpolyList system of polynomials as either a character vector or mpolyList
#' @param varorder variable order (see examples)
#' @param definitions an optional named list of the definitions to be given to
#'    Bertini. The definitions name all arguments used in the polynomial and
#'    tell Bertini what type of homotopy to use. Defaults to a total-degree homotopy.
#' @param configurations an optional named list of configurations to be given to
#'    Bertini.
#' @return an object of class bertini_input
#' @export
#'
#' @examples
#'
#' if (has_bertini()) {
#'
#' polys <- mp(c("x^2 + y^2 - 1", "x - y"))
#'
#' # no configurations, no definitions
#' bertini_input(polys)
#'
#' # adding definition_block
#' definitions <- list("variable_group" = c("x","y"),
#'                     "function" = c("fun1", "fun2"))
#' bertini_input(polys, definitions = definitions)
#'
#' configurations <- list("securitymaxnorm" = 1e8)
#' bertini_input(polys, definitions = definitions, configurations = configurations)
#' }
bertini_input <- function(mpolyList,
                          varorder,
                          definitions = list(),
                          configurations = list()
                          ) {

  if(is.character(mpolyList)) mpolyList <- mp(mpolyList)
  if(is.mpoly(mpolyList)) mpolyList <- structure(list(mpolyList), class = "mpolyList")
  stopifnot(is.mpolyList(mpolyList))

  if(!missing(varorder) && !all(sort(vars) == sort(varorder))) stop(
    "If varorder is provided, it must contain all of the variables.",
    call. = FALSE
  )

  if(!missing(varorder)) vars <- varorder

  # definitions block

  # sort out variables
  vars <- mpoly::vars(mpolyList)

  # make function names
  fun_names <- str_c("f", 1:length(mpolyList))

  if(length(definitions) == 0) {
    defs_block <- list(
      "variable_group" = vars,
      "function" = fun_names
    )
  } else {
    # check if definition names are valid
    if(!all(names(definitions) %in% valid_def_names)) {
      stop("not all definition names are valid; See valid_def_names")
    }

    # check if all variables are in the definitions
    new_defs <- definitions[!names(definitions) == "function"]

    if(!all(unlist(new_defs) %in% vars)) {
      stop("The definitions block must contain all variables")
    }

    defs_block <- definitions
  }

  # fix configuration names
  names(configurations) <- str_to_lower(names(configurations))

  # make base bertini_input structure
  struct <- list(
    config_block = configurations,
    defs_block = defs_block,
    funs_block = mpolyList
  )

  struct <- structure(struct, class = "bertini_input")
  struct
}
