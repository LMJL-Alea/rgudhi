test <- function() {
  # this should return TRUE after 'pandas' is automagically installed
  reticulate::py_module_available("gudhi")
}
