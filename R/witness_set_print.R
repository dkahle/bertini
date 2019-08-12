#' Witness Set Printing
#'
#' Print the witness set of specified component(s) of a
#' numerical irreducible decomposition.
#'
#' @param out the output of a positive-dimension run from bertini
#' @param dimension the dimension of the component's witness set
#' to be printed.
#' @param component The component's witness set to be printed. Indexing of
#' components starts at 0, not 1. To select all components of a particular
#' dimension, input -2.
#' @param dir directory where the computations will take place
#' @param ... additional configuration arguments to be changed.
#'
#' @return an object of class witness_set
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
#' witness_set_print(out, 1, -2)
witness_set_print <- function(out, dimension, component, dir = tempdir(), ...) {

  # check if out is positive-dimensional
  stopifnot(is.bertini_posdim(out))

  # change input to witness set printing
  input <- modify_config(out$bertini_code, tracktype = 4)

  # change any other configurations the user specifies
  input <- modify_config(input, ...)

  # make temp directory
  dir.create(scratch_dir <- file.path(dir, time_stamp()))

  # switch to temporary directory
  user_working_directory <- getwd()
  setwd(scratch_dir); on.exit(setwd(user_working_directory), add = TRUE)

  # make bertini file
  write_bertini(input, where = scratch_dir)

  # write witness_data to scratch_dir
  writeLines(out$witness_data, file.path(scratch_dir, "witness_data"))

  # make and write options for witness set printing
  options <- glue_collapse(c(dimension, component, "witness_points", "linear_system"), sep = "\n")
  writeLines(options, file.path(scratch_dir, "options"))

  system2(
    file.path(get_bertini_path(), "bertini"),
    glue_collapse(c(file.path(scratch_dir, "bertini_code"), "<", "options")),
    stdout = "bertini_out",
    stderr = "bertini_err"
  )

  # read in created files
  witness_points <- readLines("witness_points")
  linear_system <- readLines("linear_system")

  witness_set <- list(points = witness_points,
                      system = linear_system)
  class(witness_set) <- "witness_set"

  witness_set
}
