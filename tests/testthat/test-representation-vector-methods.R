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

# test_that("The Entropy class works", {
#   ent <- Entropy$new()
#   expect_snapshot(ent$apply(dgm))
#   expect_snapshot(ent$fit(list(dgm))$transform(list(dgm)))
#   expect_snapshot(ent$fit_transform(list(dgm)))
# })
#
# test_that("The Landscape class works", {
#   land <- Landscape$new()
#   expect_snapshot(land$apply(dgm))
#   expect_snapshot(land$fit(list(dgm))$transform(list(dgm)))
#   expect_snapshot(land$fit_transform(list(dgm)))
# })
#
# test_that("The PersistenceImage class works", {
#   pei <- PersistenceImage$new()
#   expect_snapshot(pei$apply(dgm))
#   expect_snapshot(pei$fit(list(dgm))$transform(list(dgm)))
#   expect_snapshot(pei$fit_transform(list(dgm)))
# })
#
# test_that("The Silhouette class works", {
#   sil <- Silhouette$new()
#   expect_snapshot(sil$apply(dgm))
#   expect_snapshot(sil$fit(list(dgm))$transform(list(dgm)))
#   expect_snapshot(sil$fit_transform(list(dgm)))
# })
#
# test_that("The TopologicalVector class works", {
#   tov <- TopologicalVector$new()
#   expect_snapshot(tov$apply(dgm))
#   expect_snapshot(tov$fit(list(dgm))$transform(list(dgm)))
#   expect_snapshot(tov$fit_transform(list(dgm)))
# })
