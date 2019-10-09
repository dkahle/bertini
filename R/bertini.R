#' Evaluate Bertini Code
#'
#' Write a Bertini file, evaluate it through a back-end connection to Bertini,
#' and bring the output back into R.
#'
#' @param code Bertini code as either a character string, or bertini_input object;
#'  see examples
#' @param dir directory to place the files in, without an ending /
#' @param quiet show bertini output
#' @param output the type of output expected (zero-dimensional or positive-dimensional)
#' @param parameter_homotopy logical indicating if the run is a parameter homotopy.
#' @param start_points an optional specification of the starting system in a list of points.
#'  This is usually only specified for user-defined homotopies.
#' @return an object of class bertini
#' @export bertini
#' @examples
#'
#' if (has_bertini()) {
#'
#' # where does the circle intersect the line y = x?
#' code <- "
#' INPUT
#'
#' variable_group x, y;
#' function f, g;
#'
#' f = x^2 + y^2 - 1;
#' g = y - x;
#'
#' END;
#' "
#' bertini(code)
#' c(sqrt(2)/2, sqrt(2)/2)
#' str(bertini(code), 1L, give.attr = FALSE)
#'
#'
#'
#' # where do the surfaces
#' #  x^2 - y^2 - z^2 - 1/2
#' #  x^2 + y^2 + z^2 - 9
#' #  x^2/4 + y^2/4 - z^2
#' # intersect?
#' #
#' code <- "
#' INPUT
#'
#' variable_group x, y, z;
#' function f, g, h;
#'
#' f = x^2 - y^2 - z^2 - 1/2;
#' g = x^2 + y^2 + z^2 - 9;
#' h = x^2/4 + y^2/4 - z^2;
#'
#' END;
#' "
#' bertini(code)
#'
#' # algebraic solution :
#' c(sqrt(19)/2, 7/(2*sqrt(5)), 3/sqrt(5)) # +/- each ordinate
#'
#'
#'
#'
#' # example from bertini manual
#' code <- "
#' INPUT
#'
#' variable_group x, y;
#' function f, g;
#'
#' f = x^2 - 1;
#' g = x + y - 1;
#'
#' END;
#' "
#' out <- bertini(code)
#' str(out)
#'
#'
#'
#'
#'
#' # non zero-dimensional example
#' code <- "
#' CONFIG
#'   TRACKTYPE: 1;
#' END;
#'
#' INPUT
#'   variable_group x, y, z;
#'   function f1, f2;
#'   f1 = x^2-y;
#'   f2 = x^3-z;
#' END;
#' "
#' bertini(code, output = "pos_dim")
#'
#'
#'
#' # using a bertini_input object
#'
#' polys <- mp(c("x^2 + y^2 - 1", "x - y"))
#' struct <- bertini_input(polys)
#' bertini(struct)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' }
#'
bertini <- function(code,
                    dir = tempdir(),
                    quiet = TRUE,
                    output = c("zero_dim", "pos_dim"),
                    parameter_homotopy = FALSE,
                    start_points = list()){

  output <- match.arg(output)

  # stop if bertini is not present
  if (!has_bertini()) missing_bertini_stop()

  if(!parameter_homotopy) {
      # make dir to put bertini files in (within the tempdir) timestamped
      dir.create(scratch_dir <-  file.path(dir, time_stamp()))
  } else {
    scratch_dir <- dir
  }

  # make bertini file
  write_bertini(code, where = scratch_dir)

  # if start_points exist, parse and write to file
  if(length(start_points) > 0) {

    # parse
    points <- lapply(start_points, function(x) glue_collapse(glue("{Re(x)} {Im(x)}"), sep = "\n"))
    start_file <- glue("{length(start_points)} \n\n{glue_collapse(points, sep = '\n\n')}")

    # make
    writeLines(start_file, file.path(scratch_dir, "start"))

  }


  # switch to temporary directory, run bertini
  user_working_directory <- getwd()
  setwd(scratch_dir); on.exit(setwd(user_working_directory), add = TRUE)


  # run bertini
  system2(
    file.path(get_bertini_path(), "bertini"),
    file.path(scratch_dir, "bertini_code"),
    stdout = "bertini_out",
    stderr = "bertini_err"
  )


  # print bertini output, if requested
  if(!quiet) cat(readLines("bertini_out"), sep = "\n")
  if(!quiet) std_err <- readLines("bertini_err")
  if(!quiet && any(std_err != "")) message(str_c(std_err, collapse = "\n"))


  # figure out what files to keep, and make bertini object
  files <- list.files()
  raw_output <- as.list(vector(length = length(files)))
  names(raw_output) <- files
  for(k in seq_along(files)) raw_output[[k]] <- readLines(files[k])

  if(output == "zero_dim") {
    # convert the raw output into interesting output
    out <- raw_output
    if("finite_solutions" %in% files) out$finite_solutions <- parse_bertini_finite_solutions(out)
    if("nonsingular_solutions" %in% files) out$nonsingular_solutions <- parse_bertini_nonsingular_solutions(out)
    if("singular_solutions" %in% files) out$singular_solutions <- parse_bertini_singular_solutions(out)
    if("real_finite_solutions" %in% files) out$real_finite_solutions <- parse_bertini_real_finite_solutions(out)
    if("raw_solutions" %in% files) out$raw_solutions <- parse_bertini_raw_solutions(out)
   #if("midpath_data" %in% files) out$midpath_data <- parse_bertini_midpath_data(out)
   #if("start" %in% files) out$start <- parse_bertini_start(out)
    if("failed_paths" %in% files) out$failed_paths <- parse_bertini_failed_paths(out)
    if("real_solutions" %in% files) out$real_solutions <- parse_bertini_real_solutions(out)

    # add code and directory
    out$raw_output <- raw_output
    out$bertini_code <- code
    out$dir <- scratch_dir



    # class and out
    class(out) <- "bertini"
  } else if(output == "pos_dim") {
    out <- raw_output

    out$bertini_code <- code
    out$dir <- scratch_dir

    class(out) <- "bertini_posdim"
  }
  out
}

















write_bertini <- function(code, where = tempdir(), code_file = "bertini_code"){

  # if code is in a character string
  if(is.character(code)){
    if(str_sub(code, 1, 1) != "\n") code <- paste("\n", code, sep = "")
    if(str_sub(code, -1, -1) != "\n") code <- paste(code, "\n", sep = "")
    code <- strsplit(code, "\\n")[[1]][-1]
  }

  # if code is a bertini_input object
  if(is.bertini_input(code)){
    code <- print(code, silent = TRUE)
  }

  # write code file
  writeLines(code, con = file.path(where, code_file))
  invisible(code)

}








































parse_bertini_finite_solutions <- function(rawOutput){

  # check for no finite solutions
  if(
    length(rawOutput$finite_solutions) == 1 &&
      rawOutput$finite_solutions == ""
  ) return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  fSolns <- rawOutput$finite_solutions
  fSolns <- fSolns[-c(1,2)]
  fSolns <- fSolns[-length(fSolns)]

  fSolns <- strsplit(fSolns, " ")[nchar(fSolns) > 0]
  fSolns <- vapply(fSolns, function(x){
    x <- as.numeric(x)
    complex(1, x[1], x[2])
  }, complex(1))
  fSolns <- matrix(fSolns, ncol = p, byrow = TRUE)
  colnames(fSolns) <- vars

  fSolns
}







parse_bertini_nonsingular_solutions <- function(rawOutput){

  # check for no finite solutions
  if(str_sub(rawOutput$nonsingular_solutions[1], 1, 1) == "0") return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  nsSolns <- rawOutput$nonsingular_solutions
  nsSolns <- nsSolns[-c(1,2)]
  nsSolns <- nsSolns[-length(nsSolns)]

  nsSolns <- strsplit(nsSolns, " ")[nchar(nsSolns) > 0]
  nsSolns <- vapply(nsSolns, function(x){
    x <- as.numeric(x)
    complex(1, x[1], x[2])
  }, complex(1))
  nsSolns <- matrix(nsSolns, ncol = p, byrow = TRUE)
  colnames(nsSolns) <- vars

  nsSolns
}







parse_bertini_singular_solutions <- function(rawOutput){

  # check for no finite solutions
  if(str_sub(rawOutput$singular_solutions[1], 1, 1) == "0") return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  sSolns <- rawOutput$singular_solutions
  sSolns <- sSolns[-c(1,2)]
  sSolns <- sSolns[-length(sSolns)]

  sSolns <- strsplit(sSolns, " ")[nchar(sSolns) > 0]
  sSolns <- vapply(sSolns, function(x){
    complex(1, x[1], x[2])
  }, complex(1))
  sSolns <- matrix(sSolns, ncol = p, byrow = TRUE)
  colnames(sSolns) <- vars

  sSolns
}









parse_bertini_real_finite_solutions <- function(rawOutput){

  # check for no finite solutions
  if(str_sub(rawOutput$real_finite_solutions[1], 1, 1) == "0") return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  rfSolns <- rawOutput$real_finite_solutions
  rfSolns <- rfSolns[-c(1,2)]
  rfSolns <- rfSolns[-length(rfSolns)]

  rfSolns <- strsplit(rfSolns, " ")[nchar(rfSolns) > 0]
  rfSolns <- vapply(rfSolns, function(x) as.numeric(x[1]), numeric(1))
  rfSolns <- matrix(rfSolns, ncol = p, byrow = TRUE)
  colnames(rfSolns) <- vars

  rfSolns
}










parse_bertini_raw_solutions <- function(rawOutput){

  # check for no finite solutions
  if(str_sub(rawOutput$raw_solutions[1], 1, 1) == "0") return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  rawSolns <- rawOutput$raw_solutions
  rawSolns <- rawSolns[-c(1,2)]
  rawSolns <- rawSolns[-length(rawSolns)]
  rawSolns <- rawSolns[str_detect(rawSolns, " ")]

  rawSolns <- strsplit(rawSolns, " ")
  rawSolns <- vapply(rawSolns, function(x){
    x <- as.numeric(x)
    complex(1, x[1], x[2])
  }, complex(1))
  rawSolns <- matrix(rawSolns, ncol = p, byrow = TRUE)
  colnames(rawSolns) <- vars

  rawSolns
}










parse_bertini_midpath_data <- function(rawOutput){

  # check for no finite solutions
  if( length(rawOutput$midpath_data) == 1 && rawOutput$midpath_data == "" ) return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- paste(vars, "homog")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  mdpthPts <- rawOutput$midpath_data
  mdpthPts <- mdpthPts[-length(mdpthPts)]
  mdpthPts <- mdpthPts[nchar(mdpthPts) > 0]

  pthMarkerNdx <- which(unname(sapply(mdpthPts, nchar)) == 1 | unname(sapply(mdpthPts, nchar)) == 2)
  mdpthPts <- mdpthPts[-pthMarkerNdx]
  mdpthPts <- strsplit(mdpthPts, " ")
  mdpthPts <- vapply(mdpthPts, function(x){
    x <- as.numeric(x)
    complex(1, x[1], x[2])
  }, complex(1))

  mdpthPts <- matrix(mdpthPts, ncol = p, byrow = TRUE)
  colnames(mdpthPts) <- vars

  mdpthPts
}











parse_bertini_start <- function(rawOutput){

browser()
  # check for no finite solutions
  if(
    length(rawOutput$start) == 1 &&
      rawOutput$start == ""
  ) return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- paste(vars, "homog")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  startPts <- rawOutput$start
  startPts <- startPts[-length(startPts)]
  startPts <- startPts[str_detect(startPts, " ")]
  startPts <- str_replace_all(startPts, ";", "")

  startPts <- strsplit(startPts, " ")
  startPts <- vapply(startPts, function(x){
    x <- as.numeric(x)
    complex(1, x[1], x[2])
  }, complex(1))

  startPts <- matrix(startPts, ncol = p, byrow = TRUE)
  colnames(startPts) <- vars

  startPts
}






parse_bertini_failed_paths <- function(rawOutput){

  # check for no finite solutions
  if(
    length(rawOutput$failed_paths) == 1 &&
      rawOutput$failed_paths == ""
  ) return(FALSE)

  rawOutput$failed_paths
}

parse_bertini_real_solutions <- function(rawOutput){

  # check for no finite solutions
  if(str_sub(rawOutput$real_solutions[1], 1, 1) == "0") return(FALSE)

  # get variables
  vars <- str_replace(rawOutput$main_data[2], "Variables:  ", "")
  vars <- str_split(vars, " ")[[1]]
  p <- length(vars)

  # grab output, format and return
  rSolns <- rawOutput$real_solutions
  rSolns <- rSolns[-c(1,2)]
  rSolns <- rSolns[-length(rSolns)]

  rSolns <- strsplit(rSolns, " ")[nchar(rSolns) > 0]
  rSolns <- vapply(rSolns, function(x) as.numeric(x[1]), numeric(1))
  rSolns <- matrix(rSolns, ncol = p, byrow = TRUE)
  colnames(rSolns) <- vars

  rSolns
}
