#' Remote Data Sets
#'
#' A collection of function for fetching famous remote data sets.
#'
#' @param data_folder A string specifying a location for storing data ot be used
#'   with GUDHI.
#' @param accept_license A boolean specifying whether the user accepts the file
#'   `LICENSE` and prevents from printing the corresponding license terms.
#'   Defaults to `FALSE`.
#'
#' @return A numeric array storing the points of the corresponding data set.
#' @name fetch
#'
#' @examplesIf reticulate::py_module_available("gudhi")
#' b <- withr::with_tempdir({fetch_bunny(getwd())})
#' s <- withr::with_tempdir({fetch_spiral_2d(getwd())})
NULL

#' @section Stanford bunny dataset:
#' The [fetch_bunny()] function returns a numeric array of shape \eqn{35947
#' \times 3}.
#' @export
#' @rdname fetch
fetch_bunny <- function(data_folder, accept_license = FALSE) {
  withr::local_envvar(c("GUDHI_DATA" = data_folder))
  gd$datasets$remote$fetch_bunny(accept_license = accept_license)
}

#' @section `spiral_2d` dataset:
#' The [fetch_spiral_2d()] function returns a numeric array of shape
#' \eqn{114,562 \times 2}.
#' @export
#' @rdname fetch
fetch_spiral_2d <- function(data_folder) {
  withr::local_envvar(c("GUDHI_DATA" = data_folder))
  gd$datasets$remote$fetch_spiral_2d()
}

#' @export
#' @rdname fetch
clear_data_home <- function(data_folder) {
  withr::local_envvar(c("GUDHI_DATA" = data_folder))
  gd$datasets$remote$clear_data_home()
}
