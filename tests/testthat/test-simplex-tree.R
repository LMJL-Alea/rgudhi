test_that("Class `SimplexTree` works", {
  n <- 10
  withr::with_seed(1234, {
    X <- replicate(n, runif(2), simplify = FALSE)
  })
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  st$compute_persistence()
  expect_true(all(st$betti_numbers() == c(1, 0)))
  expect_equal(st$dimension(), 2)
})
