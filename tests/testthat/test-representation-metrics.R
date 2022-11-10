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

test_that("The BottleneckDistance class works", {
  dis <- BottleneckDistance$new()
  expect_snapshot(dis$apply(dgm, dgm))
  expect_snapshot(dis$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(dis$fit_transform(list(dgm)))
})

test_that("The PersistenceFisherDistance class works", {
  skip_on_os("linux") # needs to monitor is it fixes itself or what
  dis <- PersistenceFisherDistance$new()
  expect_snapshot(dis$apply(dgm, dgm))
  expect_snapshot(dis$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(dis$fit_transform(list(dgm)))
})

test_that("The SlicedWassersteinDistance class works", {
  dis <- SlicedWassersteinDistance$new()
  expect_snapshot(dis$apply(dgm, dgm))
  expect_snapshot(dis$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(dis$fit_transform(list(dgm)))
})

test_that("The WassersteinDistance class works", {
  dis <- WassersteinDistance$new()
  expect_snapshot(dis$apply(dgm, dgm))
  expect_snapshot(dis$fit(list(dgm))$transform(list(dgm)))
  expect_snapshot(dis$fit_transform(list(dgm)))
})
