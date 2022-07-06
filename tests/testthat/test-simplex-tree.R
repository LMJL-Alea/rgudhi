# helper function to skip tests if we don't have the 'gudhi' module
skip_if_no_gudhi <- function() {
  have_gudhi <- reticulate::py_module_available("gudhi")
  if (!have_gudhi)
    skip("gudhi not available for testing")
}

test_that("Class `SimplexTree` works", {
  skip_if_no_gudhi()

  n <- 10
  withr::with_seed(1234, {
    X <- replicate(n, runif(2), simplify = FALSE)
  })
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  st$compute_persistence()
  expect_true(all(st$betti_numbers() == c(1, 0)))
  expect_equal(st$dimension(), 2)
  expect_snapshot(st$filtration(1:2))
  expect_true(st$find(1:2))
  expect_snapshot(st$get_boundaries(1:2))
})
