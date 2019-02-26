#' Set path to Bertini (bertini)
#'
#' This function sets the path to external programs either by (1) passing it a
#' character string or (2) using [file.choose()].
#'
#' When bertini is loaded it attempts to find bertini.  How it looks depends on
#' your operating system.
#'
#' If you're using a Mac or Linux machine, it looks based on your system's path.
#' Unfortunately, R changes the system path in such a way that the path that R
#' sees is not the same as the path that you'd see if you were working in the
#' terminal. (You can open the Terminal app on a Mac by going to
#' /Applications/Utilities/Terminal.)  Consequently, bertini tries to guess the
#' file in which your path is set.  To do so, it first checks if your home
#' directory (type echo ~/ in the terminal to figure out which directory this is
#' if you don't know) for the file named .bash_profile.  If this file is
#' present, it runs it and then checks your system's path variable (echo $PATH).
#' If it's not present, it does the same for .bashrc and then .profile. In any
#' case, once it has its best guess at your path, it looks for "bertini".
#'
#' On Windows, bertini just defaults to the cloud implementation. Local bertini
#' instances are not currently supported on Windows.
#'
#' @param path A character string, the path to bertini
#' @return An invisible character string, the path found.  More importantly, the
#'   function has the side effect of setting the global bertini option
#'   "bertini_path"
#' @export
#' @name bertini_path
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @examples
#'
#' \dontrun{ requires Bertini
#'
#'
#' getOption("bertini")
#' get_bertini_path()
#' set_bertini_path()
#'
#'
#' ## each of these functions can be used statically as well
#' (bertini_path <- get_bertini_path())
#' set_bertini_path("/path/to/bertini/directory")
#' get_bertini_path()
#' set_bertini_path(bertini_path) # undoes example
#'
#'
#' # if you'd like to use the cloud, after you library(bertini)
#' # and before you use bertini() type
#' set_bertini_path(NULL)
#'
#' # alternatively, if you have already been using bertini, do:
#' stop_bertini()
#' set_bertini_path(NULL)
#' bertini("1+1")
#'
#'
#' }






#' @rdname bertini_path
#' @export
set_bertini_path <- function(path = NULL){

  if(missing(path) && interactive()){

    path <- dirname(file.choose())
    if(is.win() && str_detect(path,"C:/")){
      mpath <- str_replace(dirname(path), "C:/", "/cygdrive/c/")
    }
    set_bertini_option(bertini_path = path)
    return(invisible(path))

  } else if (!missing(path)) {

    set_bertini_option(bertini_path = path)
    return(invisible(path))

  } else {
    stop(
      "If the session is not interactive, a path must be specified.",
      call. = FALSE
    )
  }
}





#' @rdname bertini_path
#' @export
get_bertini_path <- function() getOption("bertini")$bertini_path



#' @rdname bertini_path
#' @export
get_bertini_con <- function() getOption("bertini")$bertini_con



#' @rdname bertini_path
#' @export
get_bertini_procid <- function() getOption("bertini")$bertini_procid



#' @rdname bertini_path
#' @export
get_bertini_port <- function() getOption("bertini")$bertini_port







