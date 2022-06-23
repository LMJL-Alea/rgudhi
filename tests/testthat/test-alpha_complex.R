test_that("Alpha complex works", {
  n <- 10
  withr::with_seed(1234, {
    X <- replicate(n, runif(2), simplify = FALSE)
  })
  ac <- alpha_complex(points = X)
  st <- ac$create_simplex_tree()
  dgm <- st$persistence()
  expect_snapshot(dgm)
  expect_snapshot(plot_persistence_diagram(dgm))
})
