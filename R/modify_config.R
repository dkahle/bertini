#' Add, Modify, or Remove Configuration
#'
#' Add to, modify, and/or remove a configuration or multiple configurations in a
#' bertini_input object. This function is a thin wrapper of the
#' [purrr::list_modify()] function in the purrr package. Therefore, you can use
#' [rlang::zap()] to remove a configuration.
#'
#' @param x a bertini_input object
#' @param ... configurations to be added or modified.
#' @return the bertini_input object with the added, modified, or removed
#'   configurations
#' @name modify-config
#' @examples
#'
#' (struct <- bertini_input(mp(c("x^2 + y^2 - 1","x - y"))))
#'
#' modify_config(struct, precision = 124)
#'
#'
#'





#' @rdname modify-config
#' @export
modify_config <- function(x, ...) {

  # stop if not a bertini_input object
  stopifnot(is.bertini_input(x))

  configs <- list(...)

  # change everything to lower case
  names(configs) <- str_to_lower(names(configs))

  # check to see if configurations are valid
  if(!all(names(configs) %in% valid_configurations)) {
    stop("not all configuration names are valid; See valid_congurations")
  }

  # add configurations
  x$config_block <- list_modify(x$config_block, !!!configs)

  # return
  x
}
