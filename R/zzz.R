# global reference to sklearn.preprocessing (will be initialized in .onLoad)
skl_preprocessing <- NULL
# global reference to sklearn.cluster (will be initialized in .onLoad)
skl_cluster <- NULL
# global reference to gudhi (will be initialized in .onLoad)
gd <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  # use superassignment to update global reference to sklearn.preprocessing
  skl_preprocessing <<- reticulate::import("sklearn.preprocessing", delay_load = TRUE, convert = TRUE)
  # use superassignment to update global reference to sklearn.preprocessing
  skl_cluster <<- reticulate::import("sklearn.cluster", delay_load = TRUE, convert = TRUE)
  # use superassignment to update global reference to gudhi
  gd <<- reticulate::import("gudhi", delay_load = TRUE, convert = TRUE)
}
