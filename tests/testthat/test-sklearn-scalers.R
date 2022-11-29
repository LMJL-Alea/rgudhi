test_that("Scalers work", {
  withr::with_seed(1234, {
    X <- matrix(rnorm(10), 5, 2)
  })
  mas <- MaxAbsScaler$new()
  expect_snapshot(mas$get_python_class()$fit_transform(X))
  mms <- MinMaxScaler$new()
  expect_snapshot(mms$get_python_class()$fit_transform(X))
  rs <- RobustScaler$new()
  expect_snapshot(rs$get_python_class()$fit_transform(X))
  ss <- StandardScaler$new()
  expect_snapshot(ss$get_python_class()$fit_transform(X))
})
