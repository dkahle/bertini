#' Component Sampling
#'
#' Draw the selected number of samples from a specified component
#' and dimension of a numerical irreducible decomposition.
#'
#' @param out the output of a positive-dimension run from bertini
#' @param dimension the dimension of the component to be sampled from
#' @param component the component to be sampled from. Indexing of components
#' starts at 0, not 1.
#' @param n the number of samples to be drawn from the selected component
#' @param dir directory to place the files in, without an ending /
#' @param ... additional configuration arguments to be changed
#'
#' @return a matrix of samples
#' @export
#'
#' @examples
#'if (has_bertini()) {
#'
#' # see ?bertini
#'
#' # run initial numerical irreducible decomposition for equation xy = 0
#' input <- bertini_input("x*y")
#' input <- modify_config(input, tracktype = 1)
#'
#' out <- bertini(input, output = "pos_dim")
#'
#' # 25 samples from the first one-dimensional component (complex x-axis)
#' sample_component(out, dimension = 1, component = 0, n = 25)
#'}
sample_component <- function(out, dimension, component,
                             n, dir = tempdir(), ...) {

  # check if out is positive-dimensional
  stopifnot(is.bertini_posdim(out))

  # change input to component sampling
  input <- modify_config(out$bertini_code, tracktype = 2)

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

  # make and write options for component sampling
  options <- glue_collapse(c(dimension, component, n, 0, "sample_points"), sep = "\n")
  writeLines(options, file.path(scratch_dir, "options"))

  system2(
    file.path(get_bertini_path(), "bertini"),
    glue_collapse(c(file.path(scratch_dir, "bertini_code"), "<", "options")),
    stdout = "bertini_out",
    stderr = "bertini_err"
  )

  # get variables
  vars <- str_replace(out$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # parse sample_points
  samps <- readLines("sample_points")
  samps <- samps[-c(1,2)]
  samps <- strsplit(samps, " ")[nchar(samps) > 0]
  samps <- vapply(samps, function(x){
    x <- as.numeric(x)
    complex(1, x[1], x[2])
  }, complex(1))

  # make matrix
  mat <- matrix(samps, nrow = n, ncol = p, byrow = TRUE)
  colnames(mat) <- vars

  mat
}
