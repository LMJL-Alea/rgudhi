n <- 10
X <- seq_circle(n)
ac <- AlphaComplex$new(points = X)
st <- ac$create_simplex_tree()
dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)

test_that("The BirthPersistenceTransform class works", {
  bpt <- BirthPersistenceTransform$new()
  single_val <- bpt$apply(dgm)
  expect_true(inherits(single_val, "tbl_df"))
  expect_equal(ncol(single_val), 2)
  expect_equal(names(single_val), c("birth", "lifetime"))
  expect_snapshot(single_val)
  list_val <- bpt$transform(list(dgm))
  expect_true(inherits(list_val, "list"))
  expect_equal(length(list_val), 1)
  expect_true(inherits(list_val[[1]], "tbl_df"))
  expect_equal(ncol(list_val[[1]]), 2)
  expect_equal(names(list_val[[1]]), c("birth", "lifetime"))
  expect_snapshot(list_val)
  ft_val <- bpt$fit_transform(list(dgm))
  expect_true(inherits(ft_val, "list"))
  expect_equal(length(ft_val), 1)
  expect_true(inherits(ft_val[[1]], "tbl_df"))
  expect_equal(ncol(ft_val[[1]]), 2)
  expect_equal(names(ft_val[[1]]), c("birth", "lifetime"))
  expect_snapshot(ft_val)
})

test_that("The DiagramScaler class works", {
  ds <- DiagramScaler$new(use = TRUE)
  expect_equal(ds$apply(dgm), dgm)
  expect_equal(ds$transform(list(dgm)), list(dgm))
  expect_equal(ds$fit_transform(list(dgm)), list(dgm))
})

test_that("The DiagramSelector class works", {
  ds <- DiagramSelector$new(use = TRUE)
  dgm_expected <- subset(dgm, death != Inf)
  expect_equal(ds$apply(dgm), dgm_expected)
  expect_equal(ds$transform(list(dgm)), list(dgm_expected))
  expect_equal(ds$fit_transform(list(dgm)), list(dgm_expected))
})

test_that("The Padding class works", {
  pad <- Padding$new(use = TRUE)
  dgm_padded <- dgm
  dgm_padded$original <- 1
  expect_equal(pad$apply(dgm), dgm_padded)
  expect_equal(pad$transform(list(dgm)), list(dgm_padded))
  expect_equal(pad$fit_transform(list(dgm)), list(dgm_padded))
})

test_that("The ProminentPoints class works", {
  pp <- ProminentPoints$new(use = TRUE, threshold = 0.1)
  dgm_expected <- tibble::tibble(birth = 0, death = Inf)
  expect_equal(pp$apply(dgm), dgm_expected)
  expect_equal(pp$transform(list(dgm)), list(dgm_expected))
  expect_equal(pp$fit_transform(list(dgm)), list(dgm_expected))
})
