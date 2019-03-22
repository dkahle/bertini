#' Polynomial Optimization
#'
#' Find the collection of critical points of a multivariate polynomial
#' unconstrained or constrained to an affine variety (algebraic set; solution
#' set of multivariate polynomials).
#'
#' @param objective the objective polynomial (as a character or mpoly)
#' @param constraints (as a character or mpoly/mpolyList)
#' @param varorder variable order (see examples)
#' @param ... stuff to pass to bertini
#' @return an object of class bertini
#' @export poly_optim
#' @examples
#'
#' \dontrun{ requires Bertini
#'
#' # unconstrained optimization of polynomial functions is available
#' poly_optim("x^2")
#' poly_optim("-x^2")
#' poly_optim("-(x - 2)^2")
#' poly_optim("-(x^2 + y^2)")
#' poly_optim("-(x^2 + (y - 2)^2)")
#' poly_optim("-(x^2 + (y - 2)^2)") # saddle
#'
#' poly_optim("(x - 1) (x - 2) (x - 3)") # fix global labeling
#'
#'
#' # constrained optimization over the affine varieties is also available
#' # (affine variety = solution set of polynomial equations)
#'
#' # find the critical points of the plane f(x,y) = x + y
#' # over the unit circle x^2 + y^2 = 1
#' poly_optim("x + y", "x^2 + y^2 = 1")
#'
#' # you can specify them as a combo of mpoly, mpolyList, and characters
#' o <- mp("x + y")
#' c <- "x^2 + y^2 = 1"
#' poly_optim(o, c)
#'
#' c <- mp("x^2 + y^2 - 1")
#' poly_optim(o, c)
#'
#' out <- poly_optim("x + y", c)
#' str(out, 1, give.attr = FALSE)
#' cat(out$bertini_code)
#'
#' # another example, note the solutions are computed over the complex numbers
#' poly_optim("x^2 y", "x^2 + y^2 = 3")
#' # solutions: (+-sqrt(2), +-1) and (0, +-sqrt(3))
#'
#'
#'
#'
#' }
#'
poly_optim <- function(objective, constraints, varorder, ...){

  optimizationType <- "unconstrained"

  if(!missing(constraints)){

    optimizationType <- "constrained"

    if(is.character(constraints)){
      if(all(str_detect(constraints, "=="))){
        split <- strsplit(constraints, " == ")
        lhs   <- vapply(split, function(x) x[1], character(1))
        rhs   <- vapply(split, function(x) x[2], character(1))
        constraints <- mp(lhs) - mp(rhs)
      } else if(all(str_detect(constraints, "="))){
        split <- strsplit(constraints, " = ")
        lhs   <- vapply(split, function(x) x[1], character(1))
        rhs   <- vapply(split, function(x) x[2], character(1))
        constraints <- mp(lhs) - mp(rhs)
      } else {
        constraints <- mp(constraints)
      }
    }

    if(is.mpoly(constraints))
      constraints <- structure(list(constraints), class = "mpolyList")

    # add lagrange multipliers
    lams <- str_c("l", length(constraints))
    mults <- mp( lams )
    if(is.mpoly(mults)) mults <- structure(list(mults), class = "mpolyList")
    lagrangeConstraints <- mults * constraints
  } else {
    lams <- NULL
  }


  if(is.character(objective)) objective <- mp(objective)
  stopifnot(is.mpoly(objective))


  # sort out variables
  objectiveVars <- vars(objective)
  nVars <- length(objectiveVars)
  nLagrangeMults <- length(lams)
  vars <- c(objectiveVars, lams)

  if(!missing(varorder) &&
      !all(sort(objectiveVars) == sort(varorder))
  ){
    stop("if varorder is provided, it must contain all of the variables.", call. = FALSE    )
  }
  if(!missing(varorder)) vars <- varorder

  deriv.mpoly <- get("deriv.mpoly", envir = asNamespace("mpoly"))
  if(optimizationType == "unconstrained") {
    grad <- deriv.mpoly(objective, var = vars)
  } else if(optimizationType == "constrained"){
    lagrangian <- objective + Reduce("+", lagrangeConstraints)
    grad <- deriv.mpoly(lagrangian, var = vars)
  }

  out <- poly_solve(grad, varorder = vars)

  # add optim related stuff to the output
  out$variables <- list(vars = objectiveVars, lams = lams)
  f <- suppressMessages(as.function(objective, varorder = objectiveVars))
  real_optima <- as.data.frame(out$real_finite_solutions)
  real_optima$value <- apply(real_optima[,1:nVars,drop=FALSE], 1, f)
  real_optima <- real_optima[order(real_optima$value, decreasing = TRUE),]
  real_optima$optima <- ""
  real_optima$optima[which.max(real_optima$value)] <- "global max"
  real_optima$optima[which.min(real_optima$value)] <- "global min"
  if(optimizationType == "unconstrained") real_optima$optima <- ""
  out$real_optima <- real_optima

  class(out) <- c("poly_optim", "bertini")

  out
}
