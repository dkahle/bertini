<!-- README.md is generated from README.Rmd. Please edit that file -->

bertini
=======

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/bertini)](https://cran.r-project.org/package=bertini)
[![Travis build
status](https://travis-ci.org/dkahle/bertini.svg?branch=master)](https://travis-ci.org/dkahle/bertini)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dkahle/bertini?branch=master&svg=true)](https://ci.appveyor.com/project/dkahle/bertini)
<!-- badges: end -->

**bertini** is an R package that provides methods and data structures
for [numerical algebraic
geometry](https://en.wikipedia.org/wiki/Numerical_algebraic_geometry),
the numerical solution to (nonlinear) systems of polynomial equations
using homotopy continuation.

It is still experimental, but the core functionality in the
zero-dimensional case is quite stable.

*Note: the following assumes you have [Bertini](https://bertini.nd.edu)
and bertini recognizes its path.*

``` r
library("bertini")
# Loading required package: mpoly
#   Please cite bertini! See citation("bertini") for details.
```

Basic usage
-----------

``` r
code <- "
INPUT

variable_group x, y;
function f, g;

f = x^2 + y^2 - 1;
g = y - x;

END;
"
bertini(code)
# 2 solutions (x,y) found. (2 real, 0 complex)
#     (-0.707,-0.707) (R)
#     ( 0.707, 0.707) (R)
```

Solving zero-dimensional systems of polynomial equations
--------------------------------------------------------

`poly_solve()` is the basic workhorse for solving systems of polynomial
equations. For example, if we want to solve the system *y* = *x* and
*x*<sup>2</sup> + *y*<sup>2</sup> = 1, which corresponds geometrically
to the points where the identity line intersects the unit circle, we can
use:

``` r
poly_solve(c("y = x", "x^2 + y^2 = 1"), varorder = c("x", "y"))
# 2 solutions (x,y) found. (2 real, 0 complex)
#     (-0.707,-0.707) (R)
#     ( 0.707, 0.707) (R)
```

Polynomial optimization over compact varieties
----------------------------------------------

`poly_optim()` can be used to find the critical points of polynomials
over varieties. For example, if we want to find the maximum value of the
function *f*(*x*, *y*) = *x* + *y* over the unit circle:

``` r
poly_optim("x + y", "x^2 + y^2 = 1")
# 2 critical values (x,y) found.  (1 global maximum, 1 global minimum.)
#   ( 0.707, 0.707) ->  1.414  (global max)
#   (-0.707,-0.707) -> -1.414  (global min)
```

Installation
------------

-   From Github (dev version):

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/mpoly")
devtools::install_github("dkahle/bertini")
```

Acknowledgements
----------------

This material is based upon work supported by the National Science
Foundation under Grant Nos.
[1622449](https://nsf.gov/awardsearch/showAward?AWD_ID=1622449) and
[1622369](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622369).
