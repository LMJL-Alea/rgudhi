n <- 10
X <- lapply(
  seq(0, 2 * pi, len = n + 1)[1:n],
  function(.x) c(cos(.x), sin(.x))
)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
ds <- DiagramSelector$new(use = TRUE)
dgm <- ds$apply(dgm)

test_that("The PersistenceFisherKernel class works", {
  krnl <- PersistenceFisherKernel$new()
  expect_snapshot(krnl$apply(dgm, dgm))
  expect_snapshot(krnl$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(krnl$fit_transform(list(dgm)))
})

test_that("The PersistenceScaleSpaceKernel class works", {
  krnl <- PersistenceScaleSpaceKernel$new()
  expect_snapshot(krnl$apply(dgm, dgm))
  expect_snapshot(krnl$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(krnl$fit_transform(list(dgm)))
})

test_that("The PersistenceWeightedGaussianKernel class works", {
  krnl <- PersistenceWeightedGaussianKernel$new()
  expect_snapshot(krnl$apply(dgm, dgm))
  expect_snapshot(krnl$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(krnl$fit_transform(list(dgm)))
})

test_that("The PersistenceSlicedWassersteinKernel class works", {
  krnl <- PersistenceSlicedWassersteinKernel$new()
  expect_snapshot(krnl$apply(dgm, dgm))
  expect_snapshot(krnl$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(krnl$fit_transform(list(dgm)))
})
