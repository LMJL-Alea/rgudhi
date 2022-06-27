
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rgudhi`

<!-- badges: start -->

[![check-standard](https://github.com/astamm/rgudhi/workflows/R-CMD-check/badge.svg)](https://github.com/astamm/rgudhi/actions)
[![test-coverage](https://github.com/astamm/rgudhi/workflows/test-coverage/badge.svg)](https://github.com/astamm/rgudhi/actions)
[![Codecov test
coverage](https://codecov.io/gh/astamm/rgudhi/branch/master/graph/badge.svg)](https://app.codecov.io/gh/astamm/rgudhi?branch=master)
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
```
