test_that("Clustering algoritm Tomato works", {
  X <- seq_circle(100)
  cl <- Tomato$new()
  m <- c(0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
         3, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
         5, 5, 5, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2,
         2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0,
         0, 0, 0, 0)
  expect_equal(cl$fit_predict(X), m)
  cl$set_n_clusters(2)
  expect_equal(cl$get_labels(), c(rep(0, 25), rep(1, 58), rep(0, 17)))
})

test_that("Visualization works for Tomato", {
  skip_if_not_installed("vdiffr")
  skip_on_covr()
  skip_on_ci()
  X <- seq_circle(100)
  cl <- Tomato$new()
  cl$fit(X)
  vdiffr::expect_doppelganger(
    title = "Merge Tree Diagram",
    fig = cl$plot_diagram()
  )
})
