
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `rgudhi`

<!-- badges: start -->

[![R-CMD-check](https://github.com/astamm/rgudhi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/astamm/rgudhi/actions/workflows/R-CMD-check.yaml)
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
ac <- alpha_complex(points = X)
st <- ac$create_simplex_tree()
dgm <- st$persistence()
plot_persistence_diagram(dgm)
```
