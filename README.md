
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rgudhi`

<!-- badges: start -->

[![check-standard](https://github.com/LMJL-Alea/rgudhi/workflows/R-CMD-check/badge.svg)](https://github.com/LMJL-Alea/rgudhi/actions)
[![test-coverage](https://github.com/LMJL-Alea/rgudhi/workflows/test-coverage/badge.svg)](https://github.com/LMJL-Alea/rgudhi/actions)
[![Codecov test
coverage](https://codecov.io/gh/LMJL-Alea/rgudhi/branch/master/graph/badge.svg)](https://app.codecov.io/gh/LMJL-Alea/rgudhi?branch=master)
[![pkgdown](https://github.com/LMJL-Alea/rgudhi/workflows/pkgdown/badge.svg)](https://github.com/LMJL-Alea/rgudhi/actions)
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
devtools::install_github("LMJL-Alea/rgudhi")
```

## Example: Persistence diagram using an Alpha complex

``` r
library(rgudhi)
n <- 10
X <- seq_circle(n)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
st$persistence()
#> # A tibble: 13 × 3
#>    dimension  birth    death
#>        <int>  <dbl>    <dbl>
#>  1         1 0.0955   1.00  
#>  2         1 1        1     
#>  3         1 1        1     
#>  4         0 0      Inf     
#>  5         0 0        0.0955
#>  6         0 0        0.0955
#>  7         0 0        0.0955
#>  8         0 0        0.0955
#>  9         0 0        0.0955
#> 10         0 0        0.0955
#> 11         0 0        0.0955
#> 12         0 0        0.0955
#> 13         0 0        0.0955
```
