
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
X <- lapply(
  seq(0, 2 * pi, len = n), 
  function(.x) c(cos(.x), sin(.x))
)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
st$persistence()
#> # A tibble: 14 × 3
#>    dimension birth      death
#>        <int> <dbl>      <dbl>
#>  1         1 0.117   1.00e+ 0
#>  2         1 1       1.00e+ 0
#>  3         1 1       1   e+ 0
#>  4         1 1       1   e+ 0
#>  5         0 0     Inf       
#>  6         0 0       1.17e- 1
#>  7         0 0       1.17e- 1
#>  8         0 0       1.17e- 1
#>  9         0 0       1.17e- 1
#> 10         0 0       1.17e- 1
#> 11         0 0       1.17e- 1
#> 12         0 0       1.17e- 1
#> 13         0 0       1.17e- 1
#> 14         0 0       1.50e-32
```
