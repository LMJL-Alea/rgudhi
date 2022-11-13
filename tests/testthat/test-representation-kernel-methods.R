n <- 10
X <- seq_circle(n)
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
