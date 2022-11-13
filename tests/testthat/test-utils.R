test_that("seq_circle() works", {
  expect_equal(seq_circle(0), list())
  expect_equal(seq_circle(1), list(c(1, 0)))
})
