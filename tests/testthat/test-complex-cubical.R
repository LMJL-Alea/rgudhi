test_that("Class `CubicalComplex` works", {
  n <- 10
  X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
  cc <- CubicalComplex$new(top_dimensional_cells = X)
  expect_equal(cc$compute_persistence()$betti_numbers(), c(1, 0, 0))
  expect_snapshot(cc$compute_persistence()$cofaces_of_persistence_pairs())
  expect_equal(cc$dimension(), 2)
  expect_equal(cc$num_simplices(), 105)
  expect_snapshot(cc$persistence())
  expect_snapshot(cc$compute_persistence()$persistence_intervals_in_dimension(0))
  expect_equal(cc$persistent_betti_numbers(0, 1), c(1, 0, 0))
})

test_that("Class `PeriodicCubicalComplex` works", {
  n <- 10
  X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
  pcc <- PeriodicCubicalComplex$new(
    top_dimensional_cells = X,
    periodic_dimensions = c(TRUE, FALSE)
  )
  expect_equal(pcc$compute_persistence()$betti_numbers(), c(1, 1, 0))
  expect_snapshot(pcc$compute_persistence()$cofaces_of_persistence_pairs())
  expect_equal(pcc$dimension(), 2)
  expect_equal(pcc$num_simplices(), 100)
  expect_snapshot(pcc$persistence())
  expect_snapshot(pcc$compute_persistence()$persistence_intervals_in_dimension(0))
  expect_equal(pcc$persistent_betti_numbers(0, 1), c(1, 0, 0))
})
