is_mac <- function() grepl("darwin", R.version$platform)
is_win <- function() .Platform$OS.type == "windows"
is_linux <- function() (.Platform$OS.type == "unix") && (is_mac() == FALSE)
is_unix <- function() .Platform$OS.type == "unix"
is_solaris <- function() grepl("solaris", R.version$os)

`%notin%` <- function(elem, set){
  if(length(elem) > 1) return(vapply(elem, `%notin%`, logical(1), set = set))
  !(elem %in% set)
}

rhash <- function(n) {
  paste(
    sample(c(letters, LETTERS, 0:9), n, replace = TRUE),
    collapse = ""
  )
}
# rhash(10)

time_stamp <- function(){
  time_stamp <- as.character(Sys.time())
  time_stamp <- chartr("-", "_", time_stamp)
  time_stamp <- chartr(" ", "_", time_stamp)
  time_stamp <- chartr(":", "_", time_stamp)
  time_stamp <- paste0(time_stamp, "_", rhash(10))
  time_stamp
}
# time_stamp()


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
