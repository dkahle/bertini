#' Parameter Homotopies
#'
#' @param input system of polynomials as either a character vector,
#' mpolyList, or bertini_input object
#' @param parameters the parameters that define the family as a character vector
#' @param varorder (optional) order of variables in string
#' @param precomputed_solutions an optional list that contains the pertinent
#' results of an ab initio run (start and start_parameters files) for the system. Supplying
#' these files can greatly decrease the computation time.
#' @param dir directory to place the files in, without an ending /
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
#' # a sextic in one variable (from Bertini)
#' polys <- c("a0 + x*(a1 + x*(a2 + x*(a3 + x*(a4 + x*(a5 + x*a6)))))")
#'
#' generator <- parameter_homotopy(polys, parameters = c("a0","a1","a2","a3","a4","a5","a6"))
#' generator(c(1.1, 2.4, 0.8, 3.6, -0.52, -1.8, 4.4))


parameter_homotopy <- function(input,
                               parameters,
                               varorder,
                               precomputed_solutions = NULL,
                               dir = tempdir(),
                               definitions = list(),
                               configurations = list(),
                               subfunctions = list()) {


  if(!missing(varorder)) {
    # temporarily combine varorder and params
    varorder <- c(varorder, parameters)
  }

  if(is.bertini_input(input)) {
    struct <- input
  } else {
    # feed to bertini_input
    struct <- bertini_input(input,
                            varorder,
                            definitions = definitions,
                            configurations = configurations,
                            subfunctions = subfunctions)

    # set parameter_homotopy configurations
    struct <- modify_config(struct, parameterhomotopy = 1)

    # extract parameters
    struct$defs_block <- lapply(struct$defs_block, function(x) x[!x %in% parameters])

    # put in parameters line
    struct <- modify_defs(struct, parameter = parameters)
  }

  # scratch_dir
  dir.create(scratch_dir <-  file.path(dir, time_stamp()))

  # precomputed solutions?
  if(is.null(precomputed_solutions)) {

    # run bertini on structure
    out <- bertini(code = struct, dir = scratch_dir, parameter_homotopy = TRUE)

    # start file
    writeLines(out$raw_output$nonsingular_solutions, file.path(scratch_dir, "start"))
  } else {
    # start
    writeLines(precomputed_solutions$start, file.path(scratch_dir, "start"))

    # start parameters
    writeLines(precomputed_solutions$start_parameters, file.path(scratch_dir, "start_parameters"))
  }

  struct <- modify_config(struct, parameterhomotopy = 2)

  function(data) {
    real <- Re(data)
    imaginary <- Im(data)
    final_data <- glue("{real} {imaginary};")
    final_params <- glue("{length(data)} \n\n{glue_collapse(final_data, sep = '\n')}")
    writeLines(final_params, file.path(scratch_dir, "final_parameters"))

    bertini(struct, dir = scratch_dir, parameter_homotopy = TRUE)
  }
}
