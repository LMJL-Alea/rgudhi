n <- 10
X <- seq_circle(n)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
ds <- DiagramSelector$new(use = TRUE)
dgm <- ds$apply(dgm)

test_that("The Atol class works", {
  km <- KMeans$new(n_clusters = 2, random_state = 202006)
  vr <- Atol$new(quantiser = km)
  # expect_snapshot(vr$apply(dgm)) # TODO: needs a fix in Python
  expect_snapshot(vr$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(vr$fit_transform(list(dgm)))
})

test_that("The BettiCurve class works", {
  bc <- BettiCurve$new()
  expect_snapshot(bc$apply(dgm))
  expect_snapshot(bc$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(bc$fit_transform(list(dgm)))
})

test_that("The ComplexPolynomial class works", {
  cp <- ComplexPolynomial$new()
  expect_snapshot(cp$apply(dgm))
  expect_snapshot(cp$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(cp$fit_transform(list(dgm)))
})

test_that("The Entropy class works", {
  ent <- Entropy$new()
  expect_snapshot(ent$apply(dgm))
  expect_snapshot(ent$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(ent$fit_transform(list(dgm)))
})

test_that("The Landscape class works", {
  land <- Landscape$new()
  expect_snapshot(land$apply(dgm))
  expect_snapshot(land$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(land$fit_transform(list(dgm)))
})

test_that("The PersistenceImage class works", {
  pei <- PersistenceImage$new()
  expect_snapshot(pei$apply(dgm))
  expect_snapshot(pei$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(pei$fit_transform(list(dgm)))
})

test_that("The Silhouette class works", {
  sil <- Silhouette$new()
  expect_snapshot(sil$apply(dgm))
  expect_snapshot(sil$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(sil$fit_transform(list(dgm)))
})

test_that("The TopologicalVector class works", {
  tov <- TopologicalVector$new()
  expect_snapshot(tov$apply(dgm))
  expect_snapshot(tov$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(tov$fit_transform(list(dgm)))
})
