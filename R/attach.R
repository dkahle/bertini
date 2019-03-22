.onAttach <- function(...) {

  packageStartupMessage('  Please cite bertini! See citation("bertini") for details.')

  if (!has_bertini()) {
    packageStartupMessage("  - bertini was not set in .Renviron. Use set_bertini_path() to set it.")
  }

  invisible(TRUE)

}




