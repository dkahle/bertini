#' Set path to bertini
#'
#' These are helper functions that deal with pathing to bertini and asking if it
#' is present. When the bertini package is loaded it attempts to find the
#' bertini executable by looking for an environment variable indicating where it
#' is, i.e. its path as specified in your .Renviron file.
#'
#' For easiest use, you'll want to specify the path the bertini executable in
#' your ~/.Renviron file. It should look something like
#'
#' \code{BERTINI=/Applications/latte/bin}
#'
#' You can set this permanently with [edit_r_environ()].
#'
#' You can change this for the current session using [set_bertini_path()],
#' which accepts a character string or, if missing, uses [file.choose()] to let
#' you interactively; you just select an arbitrary executable.
#'
#' @param path A character string, the path to theh bertini executable
#' @return A logical(1) or character(1) containing the path.
#' @name pathing
#' @author David Kahle \email{david@@kahle.io}
#' @examples
#'
#' has_bertini()
#' if (has_bertini()) get_bertini_path()
#'
#'
#' # you can set this permanently with the following. note that you'll
#' # need to re-start the R session afterwards or simply pass the path into
#' # set_bertini_path(). see below for more details on that.
#' if (interactive()) edit_r_environ()
#'
#'
#' # you can change this in your current session with set_bertini_path() and
#' if (interactive()) set_bertini_path()
#'
#' if (had_bertini <- has_bertini()) old_bertini_path <- get_bertini_path()
#' set_bertini_path("/path/to/bertini")
#' get_bertini_path()
#'
#' if (had_bertini) set_bertini_path(old_bertini_path)
#' get_bertini_path()
#'
NULL












#' @rdname pathing
#' @export
set_bertini_path <- function(path){

  if(missing(path) && interactive()){

    bertini_path <- dirname(file.choose())
    if(is_win() && str_detect(bertini_path,"C:/")){
      bertini_path <- str_replace(dirname(bertini_path), "C:/", "/cygdrive/c/")
    }
    Sys.setenv("BERTINI" = bertini_path)
    return(invisible(bertini_path))

  } else if(!missing(path)){

    Sys.setenv("BERTINI" = path)
    return(invisible(path))

  } else {

    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )

  }
}
















#' @rdname pathing
#' @export
get_bertini_path <- function() Sys.getenv("BERTINI")



#' @rdname pathing
#' @export
has_bertini <- function() get_bertini_path() != ""



#' @rdname pathing
#' @export
missing_bertini_stop <- function() {
  stop(
    "bertini doesn't know where the bertini executable is.\n",
    "See ?set_bertini_path to learn how to set it.",
    call. = FALSE
  )
}



#' @importFrom usethis edit_r_environ
#' @export
usethis::edit_r_environ



