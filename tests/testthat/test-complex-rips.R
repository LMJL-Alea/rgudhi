test_that("Class `RipsComplex` works", {
  n <- 10
  X <- seq_circle(n)
  rc1 <- RipsComplex$new(data = X, max_edge_length = 1)
  Xm <- Reduce(rbind, X, init = numeric())
  rc2 <- RipsComplex$new(data = Xm, max_edge_length = 1)
  D <- dist(Xm)
  rc3 <- RipsComplex$new(data = D)
  st <- rc1$create_simplex_tree(max_dimension = 1)
  expect_equal(st$num_simplices(), 20)
})
