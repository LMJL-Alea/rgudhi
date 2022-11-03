n <- 10
X <- lapply(
  seq(0, 2 * pi, len = n + 1)[1:n],
  function(.x) c(cos(.x), sin(.x))
)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)

test_that("The BirthPersistenceTransform class works", {
  n <- 10
  X <- lapply(
    seq(0, 2 * pi, len = n + 1)[1:n],
    function(.x) c(cos(.x), sin(.x))
  )
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)

  bpt <- BirthPersistenceTransform$new()
  single_val <- bpt$get_value(dgm)
  expect_true(inherits(single_val, "tbl_df"))
  expect_equal(ncol(single_val), 2)
  expect_equal(names(single_val), c("birth", "death - birth"))
  expect_snapshot(single_val)
  list_val <- bpt$transform(list(dgm))
  expect_true(inherits(list_val, "list"))
  expect_equal(length(list_val), 1)
  expect_true(inherits(list_val[[1]], "tbl_df"))
  expect_equal(ncol(list_val[[1]]), 2)
  expect_equal(names(list_val[[1]]), c("birth", "death - birth"))
  expect_snapshot(list_val)
  ft_val <- bpt$fit_transform(list(dgm))
  expect_true(inherits(ft_val, "list"))
  expect_equal(length(ft_val), 1)
  expect_true(inherits(ft_val[[1]], "tbl_df"))
  expect_equal(ncol(ft_val[[1]]), 2)
  expect_equal(names(ft_val[[1]]), c("birth", "death - birth"))
  expect_snapshot(ft_val)
})

test_that("The DiagramScaler class works", {
  n <- 10
  X <- lapply(
    seq(0, 2 * pi, len = n + 1)[1:n],
    function(.x) c(cos(.x), sin(.x))
  )
  ac <- AlphaComplex$new(points = X)
  st <- ac$create_simplex_tree()
  dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)

  ds <- DiagramScaler$new()
  expect_equal(ds$get_value(dgm), dgm)
  expect_equal(ds$transform(list(dgm)), list(dgm))
  expect_equal(ds$fit_transform(list(dgm)), list(dgm))
})
