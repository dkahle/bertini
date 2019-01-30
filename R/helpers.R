is.mac <- function() str_detect(R.version$platform, "darwin")
is.win <- function() .Platform$OS.type == "windows"
is.linux <- function() (.Platform$OS.type == "unix") && (is.mac() == FALSE)
is.unix  <- function() .Platform$OS.type == "unix"
is.solaris <- function() str_detect(R.version$os, "solaris")

`%notin%` <- function(elem, set){
  if(length(elem) > 1) return(vapply(elem, `%notin%`, logical(1), set = set))
  !(elem %in% set)
}

time_stamp <- function(){
  str_replace_all(
    as.character(Sys.time()),
    "[- :]",
    "_"
  )
}


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
