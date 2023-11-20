# global reference to sklearn.preprocessing (will be initialized in .onLoad)
skl_preprocessing <- NULL
# global reference to sklearn.cluster (will be initialized in .onLoad)
skl_cluster <- NULL
# global reference to gudhi (will be initialized in .onLoad)
gd <- NULL
# global reference to tensorflow interface (will be initialized in .onLoad)
tfInterface <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  # use superassignment to update global reference to sklearn.preprocessing
  skl_preprocessing <<- reticulate::import(
    module = "sklearn.preprocessing",
    delay_load = TRUE,
    convert = TRUE
  )
  # use superassignment to update global reference to sklearn.preprocessing
  skl_cluster <<- reticulate::import(
    module = "sklearn.cluster",
    delay_load = TRUE,
    convert = TRUE
  )
  # use superassignment to update global reference to gudhi
  gd <<- reticulate::import(
    module = "gudhi",
    delay_load = TRUE,
    convert = TRUE
  )
  # use superassignment to update global reference to tf interface
  tfInterface <<- reticulate::import(
    module = "gudhi.tensorflow",
    delay_load = TRUE,
    convert = TRUE
  )
  tf <<- tfInterface$perslay$tf
}
