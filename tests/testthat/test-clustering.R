test_that("Clustering algoritm Tomato works", {
  withr::with_seed(1234, {
    theta <- runif(100, min = 0, max = 2*pi)
    X <- purrr::map(theta, ~ c(cos(.x), sin(.x)))
  })
  cl <- Tomato$new()
  expect_equal(length(unique(cl$fit_predict(X))), 6)
  cl$set_n_clusters(2)
  expect_equal(length(unique(cl$get_labels())), 2)
})

test_that("Visualization works for Tomato", {
  skip_if_not_installed("vdiffr")
  skip_on_ci()
  withr::with_seed(1234, {
    theta <- runif(100, min = 0, max = 2*pi)
    X <- purrr::map(theta, ~ c(cos(.x), sin(.x)))
  })
  cl <- Tomato$new()
  cl$fit(X)
  vdiffr::expect_doppelganger(
    title = "Merge Tree Diagram",
    fig = cl$plot_diagram()
  )
})
