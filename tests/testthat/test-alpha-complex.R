test_that("Class `AlphaComplex` works with list input", {
  n <- 10
  withr::with_seed(1234, {
    X <- replicate(n, runif(2), simplify = FALSE)
  })
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  expect_true(all(ac$get_point(0) == X[[1]]))
})

test_that("Class `AlphaComplex` works with matrix input", {
  n <- 10
  withr::with_seed(1234, {
    X <- cbind(runif(n), runif(2))
  })
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  expect_true(all(ac$get_point(0) == X[1, ]))
})
