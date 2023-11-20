#' Main TensorFlow module
#'
#' Interface to main TensorFlow  module. Provides access to top level classes
#' and functions as well as sub-modules (e.g. \code{tf$nn},
#' \code{tf$contrib$learn}, etc.).
#'
#' @format TensorFlow module
#'
#' @examplesIf reticulate::py_module_available("tensorflow")
#' hello <- tf$constant('Hello, TensorFlow!')
#' zeros <- tf$Variable(tf$zeros(tf$shape(1L)))
#' tf$print(hello)
#' tf$print(zeros)
#' @export
tf <- NULL
