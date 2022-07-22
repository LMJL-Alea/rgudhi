test_that("Class `CoverComplex` works", {
  cc <- CoverComplex$new(type = "GIC")
  n <- 10
  X <- lapply(
    seq(0, 2 * pi, len = n),
    function(.x) c(cos(.x), sin(.x))
  )
  X <- Reduce(rbind, X, init = numeric())
  cc$set_point_cloud_from_range(X)
  cc$read_point_cloud("https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off")
  st <- cc$create_simplex_tree()
  cc$set_verbose(TRUE)
  cc$set_verbose(FALSE)
  cc$set_gain(0.3)
  cc$set_graph_from_automatic_rips()
  cc$set_function_from_coordinate()
  cc$set_color_from_coordinate()
  cc$set_resolution_with_interval_number(100)
  cc$set_cover_from_function()
  cc$set_automatic_resolution()
  cc$find_simplices()
  cc$set_mask(1)
  cc$compute_PD()
  cc$compute_distribution()
  expect_equal(round(cc$compute_p_value(), 2), 1)
  expect_equal(round(cc$compute_confidence_level_from_distance(0.1), 2), 0.19)
  expect_equal(round(cc$compute_distance_from_confidence_level(0.95), 2), 0.15)
  expect_equal(
    cc$subpopulation(0),
    c(1161, 107, 154, 155, 1144, 108, 105, 1201,
      110, 153, 152, 106, 93, 1160, 159, 158, 1210)
  )
  withr::with_tempdir({
    cc$plot_dot()
  })
  withr::with_tempdir({
    cc$plot_off()
  })
  withr::with_tempdir({
    cc$write_info()
  })
})
