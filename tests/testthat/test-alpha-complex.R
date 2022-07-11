test_that("Class `AlphaComplex` works", {
  n <- 10
  Xl <- lapply(
    seq(0, 2 * pi, len = n),
    function(.x) c(cos(.x), sin(.x))
  )
  Xm <- Reduce(rbind, Xl, init = numeric())
  acm <- AlphaComplex$new(points = Xm)
  ac <- AlphaComplex$new(points = Xl)
  st <- ac$create_simplex_tree()
  expect_true(all(ac$get_point(0) == Xl[[1]]))
})
