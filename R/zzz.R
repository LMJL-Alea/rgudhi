# global reference to gudhi (will be initialized in .onLoad)
gd <- NULL
# global reference to gudhi.representations module (will be initialized in .onLoad)
gdr <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  # use superassignment to update global reference to gudhi
  gd <<- reticulate::import("gudhi", delay_load = TRUE, convert = TRUE)
  # use superassignment to update global reference to gudhi.representations
  gdr <<- reticulate::import("gudhi.representations", delay_load = TRUE, convert = TRUE)
}
