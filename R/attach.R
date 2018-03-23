.onAttach <- function(...) {

  packageStartupMessage('  Please cite bertini! See citation("bertini") for details.')

  # find bertini on a Mac or Linux
  if (is.mac() || is.linux()) {
    unix_search_and_set("bertini", "bertini_path")
  }

  # find bertini on a PC - directs to cloud immediately
  if (is.win()) win_search_and_set("bertini")

  # check that the programs were found
  startup_check_for_program()

  # set gmp
  set_bertini_option(gmp = FALSE)

  # return
  invisible(TRUE)
}




.onDetach <- function(...) {
  options(bertini = NULL)
}



# unix_find looks for a specific executable in a specific directory
# (or its children)
# however, we don't just use this on / because it'd take forever
# so unix_search_and_set uses unix_find to search specific directories
unix_find <- function(exec, where){

  # query the system and clean attributes
  query <- glue("find {where} -name {exec}")
  finding <- suppressWarnings(system(query, intern = TRUE, ignore.stderr = TRUE))
  attributes(finding) <- NULL

  # get the bin first
  path <- stringr::str_extract(finding, glue("[\\w/]*bin/{exec}"))

  # return
  path
}




startup_check_for_program <- function() {

  if(!is.null(get_bertini_path())){
    psm(glue("  Bertini found in {get_bertini_path()}"))
    return(invisible(FALSE))
  }

  if(is.null(get_bertini_path())){
    psm("  Bertini not found.")
    psm("  Use set_bertini_path(\"/path/to/bertini\") to run bertini locally.")
    return(invisible(FALSE))
  }

  invisible(TRUE)

}




psm  <- packageStartupMessage
psms <- function(fmt, ...) packageStartupMessage(sprintf(fmt, ...))


setOption <- function(optionName, value){
  eval(parse(text = sprintf('options("%s" = "%s")', optionName, value)))
}


unix_search_and_set <- function(exec, optionName){

  # grab path and parse
  profile_to_look_for <-
    if(file.exists("~/.bash_profile")){
      ".bash_profile"
    } else if(file.exists("~/.bashrc")){
      ".bashrc"
    } else if(file.exists("~/.profile")){
      ".profile"
    } else {
      return(invisible(FALSE))
    }

  # PATH <- system(sprintf("source ~/%s; echo $PATH", profile_to_look_for), intern = TRUE)
  # the above doesn't work on ubuntu, which uses the dash shell (which doesn't have source)
  PATH <- system(
    glue("echo 'source ~/{profile_to_look_for}; echo $PATH' | /bin/bash"),
    intern = TRUE
  )
  dirs_to_check <- stringr::str_split(PATH, ":")[[1]]

  # seek and find
  for (dir in dirs_to_check) {
    found_path <- unix_find(exec, dir)
    if(length(found_path) > 0) break
  }

  # break in a failure
  if(length(found_path) == 0) return(invisible(FALSE))

  # set option and exit
  set_bertini_option(bertini_path = dirname(found_path))

  # invisibly return path
  invisible(dirname(found_path))
}





whereis_is_accessible <- function() unname(Sys.which("whereis")) != ""

win_find <- function(s){
  wexe <- unname(Sys.which("whereis"))
  x <- system(paste(wexe, s), intern = TRUE)
  str_sub(x, nchar(s) + 2L)
}

win_search_and_set <- function(optionName) set_bertini_option(bertini_path = NULL)





# set_bertini_option both sets options for bertini in the list bertini in options
# and initialized the list when bertini is attached to the search path
# (search())
set_bertini_option <- function(...) {

  # if there is no bertini option (package is being initialized)
  # create the list with the arguments and return
  if ("bertini" %notin% names(options())) {
    options("bertini" = list(...))
    return(invisible())
  }

  # otherwise, go through arguments sequentially and add/update
  # them in the list bertini in options
  bertini <- getOption("bertini")
  arg_list <- lapply(as.list(match.call())[-1], eval, envir = parent.frame())
  for (k in seq_along(arg_list)) {
    if (names(arg_list)[k] %in% names(bertini)) {
      bertini[names(arg_list)[k]] <- arg_list[k]
    } else {
      bertini <- c(bertini, arg_list[k])
    }
  }

  # set new bertini
  options("bertini" = bertini)

  # return
  invisible()
}










