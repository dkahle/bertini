#' Membership Testing
#'
#' Test if specified points lie on components of a numerical
#' irreducible decomposition.
#'
#' @param out the output of a positive-dimension run from bertini
#' @param points a matrix of points to be tested
#' @param dir directory to place the files in, without an ending /
#' @param ... additional configuration arguments to be changed
#'
#' @return a 'glue' object that contains the decision of each point
#' @export
#'
#' @examples
#'
#' if (has_bertini()) {
#'
#' # see ?bertini
#'
#' # run initial numerical irreducible decomposition for equation xy = 0
#' input <- bertini_input("x*y")
#' input <- modify_config(input, tracktype = 1)
#'
#' out <- bertini(input, output = "pos_dim")
#'
#' # test if the points (0,0), (1,0), (0,1), (1,1) are in the solution set
#' pts_to_test <- matrix(c(0,0,1,0,0,1,1,1), nrow = 4, ncol = 2, byrow = TRUE)
#' membership_test(out, pts_to_test)
#'}
membership_test <- function(out, points, dir = tempdir(), ...) {

  # check if out is positive-dimensional
  stopifnot(is.bertini_posdim(out))

  # change input to membership testing
  input <- modify_config(out$bertini_code, tracktype = 3)

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

  # make member_points file

  # number of points to test
  n <- nrow(points)

  # matrix to bertini form
  points <- apply(points, 1, function(x) {
    real <- Re(x)
    imaginary <- Im(x)
    glue_collapse(glue("{real} {imaginary}"), sep = "\n")
  })
  points <- glue_collapse(points, sep = "\n\n")

  # member_points
  member_points <- glue("{n} \n\n {points}")
  writeLines(member_points, file.path(scratch_dir, "member_points"))

  # call bertini
  system2(
    file.path(get_bertini_path(), "bertini"),
    glue_collapse(file.path(scratch_dir, "bertini_code")),
    stdout = "bertini_out",
    stderr = "bertini_err"
  )

  # we're interested in the output on the screen
  output <- readLines("bertini_out")

  # find interesting part of output
  index_start <- which(str_detect(output, "Testing membership"))
  index_end <- which(str_detect(output, "Witness Set Decomposition"))

  interesting_output <- output[index_start:(index_end - 1)]

  glue_collapse(interesting_output, sep = "\n")
}
