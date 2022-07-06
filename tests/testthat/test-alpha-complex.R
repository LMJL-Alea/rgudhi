# helper function to skip tests if we don't have the 'gudhi' module
skip_if_no_gudhi <- function() {
  have_gudhi <- reticulate::py_module_available("gudhi")
  if (!have_gudhi)
    skip("gudhi not available for testing")
}

test_that("Class `AlphaComplex` works with list input", {
  skip_if_no_gudhi()

  n <- 10
  withr::with_seed(1234, {
    X <- replicate(n, runif(2), simplify = FALSE)
  })
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  expect_true(all(ac$get_point(0) == X[[1]]))
})

test_that("Class `AlphaComplex` works with matrix input", {
  skip_if_no_gudhi()

  n <- 10
  withr::with_seed(1234, {
    X <- cbind(runif(n), runif(2))
  })
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  expect_true(all(ac$get_point(0) == X[1, ]))
})
