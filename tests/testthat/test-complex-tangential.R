test_that("Class `TangentialComplex` works", {
  n <- 10
  X <- seq_circle(n)
  tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
  tc$set_max_squared_edge_length(1)
  tc$compute_tangential_complex()
  expect_equal(tc$num_inconsistent_simplices(), 0)
  expect_equal(tc$num_inconsistent_stars(), 0)
  expect_equal(tc$num_simplices(), 20)
  expect_equal(tc$num_vertices(), 10)
  st <- tc$create_simplex_tree()
  expect_equal(tc$get_point(0), X[[1]])
})
