#' Modify Definition(s) in a Bertini_Input Object
#'
#' @param x a bertini_input object
#' @param ... the definitions to be modified. If you
#' change any part of the variable groupings, you must fully
#' specify the variable groupings
#'
#' @return the bertini_input object with the modified definitions
#' @export
#'
#' @examples
#'
#' if (has_bertini()) {
#'   polys <- mp(c("x^2 + y^2 - 1", "x - y"))
#'   input <- bertini_input(polys)
#'
#'   modify_defs(input, variable_group = "x", variable_group = "y")
#' }
modify_defs <- function(x, ...) {

  # stop if not a bertini_input object
  stopifnot(is.bertini_input(x))

  defs <- list(...)

  # check to see if definitions are valid
  if(!all(names(defs) %in% valid_def_names)) {
    stop("not all definition names are valid; See valid_def_names")
  }


  if(any(names(defs) == "variable_group" | names(defs) == "hom_variable_group")) {

    # not having to do with variable_group or hom_variable_group
    sub_def <- defs[names(defs) != "variable_group" & names(defs) != "hom_variable_group"]
    sub_block <- x$defs_block[names(x$defs_block) != "variable_group" & names(x$defs_block) != "hom_variable_group"]
    sub_modified <- list_modify(sub_block, !!!sub_def)

    sub_vars <- defs[names(defs) == "variable_group" | names(defs) == "hom_variable_group"]
    x$defs_block <- c(sub_vars, sub_modified)
  } else {
    x$defs_block <- list_modify(x$defs_block, !!!defs)
  }
  x
}
