#' Representation Module - Abstract Base Class
#'
#' @param diag A [tibble::tibble] with two variables `birth` and `death`
#'   specifying a persistence diagram.
#' @param X A list of [tibble::tibble]s specifying a sample of persistence
#'   diagrams.
#' @param y An integer vector specifying persistence diagram labels (unused for
#'   now).
#'
#' @author Mathieu Carrière
#' @keywords internal
RepresentationBaseClass <- R6::R6Class(
  classname = "RepresentationBaseClass",
  inherit = SKLearnClass,
  public = list(
    #' @description Applies the class on a single persistence diagram and output
    #'   the result.
    get_value = function(diag) {
      super$get_value(diag) |>
        `colnames<-`(private$var_names) |>
        tibble::as_tibble()
    },

    #' @description Fits the class on a list of persistence diagrams. It might
    #'   do nothing in some cases but is useful when the class is included in a
    #'   **scikit-learn** pipeline.
    fit = function(X, y = NULL) {
      super$fit(X, y)
    },

    #' @description Applies the class on the persistence diagrams.
    transform = function(X) {
      super$transform(X) |>
        purrr::map(`colnames<-`, value = private$var_names) |>
        purrr::map(tibble::as_tibble)
    },

    #' @description Applies sequentially the `$fit()` and `$transform()` methods
    #'   on the persistence diagrams.
    fit_transform = function(X, y = NULL) {
      super$fit_transform(X, y) |>
        purrr::map(`colnames<-`, value = private$var_names) |>
        purrr::map(tibble::as_tibble)
    }
  ),
  private = list(
    var_names = NULL,
    set_var_names = function(val) {
      private$var_names <- val
    }
  )
)

#' Representation Module - Birth Persistence Transform
#'
#' @description This is a class for the affine transformation \eqn{(x,y) \mapsto
#'   (x,y-x)} to be applied on persistence diagrams.
#'
#' @author Mathieu Carrière
#' @export
BirthPersistenceTransform <- R6::R6Class(
  classname = "BirthPersistenceTransform",
  inherit = RepresentationBaseClass,
  public = list(
    #' @description The [`BirthPersistenceTransform`] constructor.
    #'
    #' @return An object of class [`BirthPersistenceTransform`].
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n + 1)[1:n],
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
      super$set_var_names(c("birth", "death - birth"))
      super$set_python_class(
        gdr$BirthPersistenceTransform()
      )
    }
  )
)

#' Representation Module - Diagram Scaler
#'
#' @description This is a class for preprocessing persistence diagrams with a
#'   given list of scalers, such as those included in **scikit-learn**.
#'
#' @author Mathieu Carrière
#' @export
DiagramScaler <- R6::R6Class(
  classname = "DiagramScaler",
  inherit = RepresentationBaseClass,
  public = list(
    #' @description The [`DiagramScaler`] constructor.
    #'
    #' @param use A boolean value specifying whether to use the class. Defaults
    #'   to `FALSE`.
    #' @param scalers A list of scalers to be fit on the persistence diagrams.
    #'   Defaults to `list()` which is an empty list. Each element of the list
    #'   is a tuple with two elements:
    #'   - the first one is a list of coordinates;
    #' - the second one is a scaler (i.e. a class with `$fit()` and
    #' `$transform()` methods) that is going to be applied to these coordinates.
    #' Common scalers can be found in the **scikit-learn** library (such as
    #' `MinMaxScaler` for instance).
    #'
    #' @return An object of class [`DiagramScaler`].
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n + 1)[1:n],
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #'   ds <- DiagramScaler$new()
    #'   ds$get_value(dgm)
    #'   ds$fit_transform(list(dgm))
    #' }
    initialize = function(use = FALSE, scalers = list()) {
      super$set_var_names(c("birth", "death"))
      super$set_python_class(
        gdr$DiagramScaler(use = use, scalers = scalers)
      )
    }
  )
)
