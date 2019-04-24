#' Pretty Printing of Bertini_Input Objects
#'
#' This function parses and views the bertini_input file as a valid Bertini
#' input file.
#'
#' @param x an object of class bertini_input
#' @param silent logical; if TRUE, suppresses output
#'
#' @return Invisible string of the printed object.
#' @export
#'
#' @examples
#'
#' if (has_bertini()) {
#'
#' bertini_input(mp(c("x^2 + y^2 - 1", "x - y")))
#'
#' }
print.bertini_input <- function(x, silent = FALSE) {

  stopifnot(is.bertini_input(x))

  if(is.null(unlist(x$config_block))) {
    config_collapse <- ""
  } else {
    config_vec <- unlist(x$config_block)
    config_collapse <- glue_collapse(glue("{names(config_vec)}: {config_vec};"), sep = "\n")
  }

  # parse definitions block
  def_vec <- map_chr(x$defs_block, glue_collapse, sep = ", ")
  def_collapse <- glue_collapse(glue("{names(def_vec)} {def_vec};"), sep = "\n")

  # parse subfunctions
  if(is.null(unlist(x$subfun_block))) {
    subfun_collapse <- ""
  } else {
    subfun_vec <- unlist(x$subfun_block)
    subfun_collapse <- glue_collapse(glue("{names(subfun_vec)} = {subfun_vec};"), sep = "\n")
  }
  # parse functions
  funs <- print(x$funs_block, stars = TRUE, silent = TRUE)
  funs <- str_replace_all(funs, "  ", " ")
  funs <- str_replace_all(funs, "\\*\\*", "^")
  funs_collapse <- glue_collapse(glue("{x$defs_block$'function'} = {funs};"), sep = "\n")

  combine <- glue("CONFIG \n {config_collapse} \nEND; \n\nINPUT \n{def_collapse} \n\n{subfun_collapse}\n{funs_collapse} \nEND;")

  if(!silent) cat(combine)

  invisible(combine)
}
