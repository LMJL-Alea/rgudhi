
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rgudhi`

<!-- badges: start -->

[![check-standard](https://github.com/astamm/rgudhi/workflows/R-CMD-check/badge.svg)](https://github.com/astamm/rgudhi/actions)
[![test-coverage](https://github.com/astamm/rgudhi/workflows/test-coverage/badge.svg)](https://github.com/astamm/rgudhi/actions)
[![Codecov test
coverage](https://codecov.io/gh/astamm/rgudhi/branch/master/graph/badge.svg)](https://app.codecov.io/gh/astamm/rgudhi?branch=master)
[![pkgdown](https://github.com/astamm/rgudhi/workflows/pkgdown/badge.svg)](https://github.com/astamm/rgudhi/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rgudhi)](https://CRAN.R-project.org/package=rgudhi)
<!-- badges: end -->

The goal of `rgudhi` is to provide an R interface to the Python package
[gudhi](https://gudhi.inria.fr/python/latest/). The
[GUDHI](https://gudhi.inria.fr) library is a generic open source C++
library, with a Python interface, for Topological Data Analysis (TDA)
and Higher Dimensional Geometry Understanding. The library offers
state-of-the-art data structures and algorithms to construct simplicial
complexes and compute persistent homology.

## Installation

You can install the development version of `rgudhi` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("astamm/rgudhi")
```

## Example: Persistence diagram using an Alpha complex

``` r
library(rgudhi)
n <- 20
X <- replicate(n, runif(2), simplify = FALSE)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
dgm <- st$persistence()
dgm
#> # A tibble: 33 × 3
#>    dimension  birth  death
#>        <int>  <dbl>  <dbl>
#>  1         1 0.0363 0.0480
#>  2         1 0.0284 0.0397
#>  3         1 0.0247 0.0314
#>  4         1 0.0297 0.0338
#>  5         1 0.0250 0.0292
#>  6         1 0.0196 0.0224
#>  7         1 0.0200 0.0220
#>  8         1 0.0379 0.0393
#>  9         1 0.0462 0.0475
#> 10         1 0.0307 0.0316
#> # … with 23 more rows
```
