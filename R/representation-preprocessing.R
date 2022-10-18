#' Abstract Class for Representation Preprocessing Module
#'
#' @author Mathieu Carrière
#'
#' @keywords internal
RepresentationPreprocessingBaseClass <- R6::R6Class(
  classname = "RepresentationPreprocessingBaseClass",
  inherit = SKLearnClass,
  public = list(
    #' @param diag A [tibble::tibble] with two variables `birth` and `death`
    #'   specifying a persistence diagram.
    get_value = function(diag) {
      super$get_value(diag)
    },
    #' @param X A list of [tibble::tibble]s specifying a sample of persistence
    #'   diagrams.
    #' @param y An integer vector specifying persistence diagram labels (unused for
    #'   now).
    fit = function(X, y = NULL) {
      super$fit(X, y)
    },
    #' @param X A list of [tibble::tibble]s specifying a sample of persistence
    #'   diagrams.
    transform = function(X) {
      super$transform(X)
    }
  )
)

#' Class for Birth Persistence Transform
#'
#' @description This is a class for the affine transformation \eqn{(x,y) \mapsto (x,y-x)} to
#' be applied on persistence diagrams.
#'
#' @param diag A [tibble::tibble] with two variables `birth` and `death` specifying a persistence diagram.
#' @param X A list of [tibble::tibble]s specifying a sample of persistence diagrams.
#' @param y An integer vector specifying persistence diagram labels (unused for now).
#'
#' @author Mathieu Carrière
#'
#' @export
BirthPersistenceTransform <- R6::R6Class(
  classname = "BirthPersistenceTransform",
  inherit = RepresentationPreprocessingBaseClass,
  public = list(
    #' @description The [`BirthPersistenceTransform`] constructor.
    #'
    #' @return A [`BirthPersistenceTransform`] object.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #'   bpt <- BirthPersistenceTransform$new()
    #'   bpt$get_value(dgm)
    #'   bpt$fit_transform(list(dgm))
    #' }
    initialize = function() {
      super$set_python_class(
        gdr$BirthPersistenceTransform()
      )
    }
  )
)
