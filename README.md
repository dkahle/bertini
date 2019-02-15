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

It is currently being developed.

*Note: the following assumes you have [Bertini](https://bertini.nd.edu)
and bertini recognizes its path.*

``` r
library("bertini")
# Loading required package: mpoly
#   Please cite bertini! See citation("bertini") for details.
#   Bertini found in /usr/local/bin
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
