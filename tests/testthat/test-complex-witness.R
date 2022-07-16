test_that("Class `WitnessComplex` works", {
  withr::with_seed(1234, {
    l <- list(
      tibble::tibble(
        nearest_landmark = sample.int(10),
        distance = sort(stats::rexp(10))
      ),
      tibble::tibble(
        nearest_landmark = sample.int(10),
        distance = sort(stats::rexp(10))
      )
    )
  })
  wc <- WitnessComplex$new(nearest_landmark_table = l)
  st <- wc$create_simplex_tree()
  expect_equal(st$num_simplices(), 1023)
})

test_that("Class `StrongWitnessComplex` works", {
  withr::with_seed(1234, {
    l <- list(
      tibble::tibble(
        nearest_landmark = sample.int(10),
        distance = sort(stats::rexp(10))
      ),
      tibble::tibble(
        nearest_landmark = sample.int(10),
        distance = sort(stats::rexp(10))
      )
    )
  })
  swc <- StrongWitnessComplex$new(nearest_landmark_table = l)
  st <- swc$create_simplex_tree()
  expect_equal(st$num_simplices(), 1023)
})
