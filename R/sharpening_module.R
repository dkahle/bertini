#' Endpoint Sharpening and Postprocessing Module
#'
#' @param out The output of a zero-dimensional Bertini run
#' @param dir directory to place the files in, without an ending /
#' @param quiet show bertini output
#' @param ... additional configuration arguments to be changed.
#'
#' @return an object of class bertini
#' @export
#'
#' @examples
#'
#' # Examples coming soon!
sharpening_module <- function(out, dir = tempdir(), quiet = TRUE, ...){

  # stop if not a bertini object
  stopifnot(is.bertini(out))

  # is R running interactively?
  if(!interactive()) {
    warning("This function should only run in an interactive R session")
  }

  # run the interactive menu
  choices_file <- interactive_menu(out)

  # make temp directory
  dir.create(scratch_dir <- file.path(dir, time_stamp()))

  # switch to temporary directory
  user_working_directory <- getwd()
  setwd(scratch_dir); on.exit(setwd(user_working_directory), add = TRUE)

  # change input to sharpening module
  input <- modify_config(out$bertini_code, sharpenonly = 1)

  # change any other configurations the user specifies
  input <- modify_config(input, ...)

  # write input file to the directory
  write_bertini(input, where = scratch_dir)

  # write the choices file to the directory
  writeLines(choices_file, file.path(scratch_dir, "options"))

  # write the raw_data file to the directory
  writeLines(out$raw_data, file.path(scratch_dir, "raw_data"))

  system2(
    file.path(get_bertini_path(), "bertini"),
    glue_collapse(c(file.path(scratch_dir, "bertini_code"), "<", "options")),
    stdout = "bertini_out",
    stderr = "bertini_err"
  )

  if(!quiet) {
    # reads back in the output
    output <- readLines("bertini_out")
    print(glue_collapse(output, sep = "\n"))
  }

  files <- list.files()
  raw_output <- as.list(vector(length = length(files)))
  names(raw_output) <- files
  for(k in seq_along(files)) raw_output[[k]] <- readLines(files[k])

  out <- raw_output
  if("finite_solutions" %in% files) out$finite_solutions <- parse_bertini_finite_solutions(out)
  if("nonsingular_solutions" %in% files) out$nonsingular_solutions <- parse_bertini_nonsingular_solutions(out)
  if("singular_solutions" %in% files) out$singular_solutions <- parse_bertini_singular_solutions(out)
  if("real_finite_solutions" %in% files) out$real_finite_solutions <- parse_bertini_real_finite_solutions(out)
  if("raw_solutions" %in% files) out$raw_solutions <- parse_bertini_raw_solutions(out)

  # add raw_output and directory
  out$raw_output <- raw_output
  out$dir <- scratch_dir



  # class and out
  class(out) <- "bertini"
  print(out)
}


interactive_menu <- function(out) {

  # need list of choices and a counter
  choices <- list()
  counter <- 1

  choices[[counter]] <- menu(c("Sharpen all endpoints",
                               "Sharpen endpoints listed in file",
                               "Manually input endpoints to sharpen",
                               "Recreate output (i.e., run the post-processor)",
                               "Change the number of sharpening digits (currently 14 digits)",
                               "Change the sharpening method (currently Newton's method)",
                               "Quit"
  ), title = "Sharpening Options")

  if(choices[[counter]] == 1) {
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  } else if(choices[[counter]] == 2) {
    counter <- counter + 1
    file_present <- FALSE
    while (file_present == FALSE) {
      cat("Please input the name of the file that lists the endpoints \n to sharpen or type 'quit' or 'exit' (max of 255 characters):")
      file_name <- readline(prompt = "Input: ")
      if(file_name == "quit" || file_name == "exit"){
        file_present <- TRUE
      } else if(file_name %in% list.files(dir)){
        file_present <- TRUE
      } else {
        cat(glue("A file named \"{file_name}\" does not exist! \n\n\n"))
      }
    }
    choices[[counter]] <- file_name
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  } else if(choices[[counter]] == 3) {
    counter <- counter + 1
    paths <- as.numeric(out$raw_output$raw_solutions[1])

    # only way to stop is -1 to quit
    path_stop <- FALSE
    while (path_stop == FALSE) {
      cat(glue("The path numbers range from 0 to {paths - 1}.\n Please enter a path number to sharpen \n (-1 to quit. Does not allow -9 option that exists in Bertini)\n"))
      choice <- as.numeric(readline(prompt = "Input: "))
      choices[[counter]] <- choice
      counter <- counter + 1

      if(choice == -1) {
        path_stop <- TRUE
      }
    }
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  } else if(choices[[counter]] == 4) {
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  } else if(choices[[counter]] == 5) {
    counter <- counter + 1
    cat("Please input the number of sharpening digits \n(currently 14 digits)\n")
    choices[[counter]] <- readline(prompt = "Input: ")
    counter <- counter + 1
    choices[[counter]] <- interactive_menu(out)
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  } else if(choices[[counter]] == 6) {
    counter <- counter + 1
    choices[[counter]] <- menu(c("Newton's method", "Endgame"), title = "Sharpening Method")
    counter <- counter + 1
    choices[[counter]] <- interactive_menu(out)
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  } else if(choices[[counter]] == 7) {
    choice_file <- glue_collapse(unlist(choices), sep = "\n")
  }
  choice_file
}
