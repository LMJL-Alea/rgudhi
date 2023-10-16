n <- 10
X <- seq_circle(n)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
ds <- DiagramSelector$new(use = TRUE)
dgm <- ds$apply(dgm)

test_that("The Atol class works", {
  km <- KMeans$new(n_clusters = 3, random_state = 1234)
  cls <- Atol$new(quantiser = km)
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})

test_that("The BettiCurve class works", {
  cls <- BettiCurve$new()
  expect_error(cls$get_python_class()$grid_)
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_error(cls$get_python_class()$grid_)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
  expect_equal(as.numeric(cls$get_python_class()$grid_), out$Grid)
})

test_that("The ComplexPolynomial class works", {
  cls <- ComplexPolynomial$new()
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})

test_that("The Entropy class works", {
  cls <- Entropy$new(mode = "vector")
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})

test_that("The Landscape class works", {
  cls <- Landscape$new()
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})

test_that("The PersistenceImage class works", {
  cls <- PersistenceImage$new()
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})

test_that("The Silhouette class works", {
  cls <- Silhouette$new()
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})

test_that("The TopologicalVector class works", {
  cls <- TopologicalVector$new()
  out <- cls$apply(dgm)
  expect_snapshot(out)
  expect_equal(cls$fit(list(dgm))$transform(list(dgm)), out)
  expect_equal(cls$fit_transform(list(dgm)), out)
})
