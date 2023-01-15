#' Preprocessing Step
#'
#' @param diag A 2-column [tibble::tibble] specifying a persistence diagram.
#' @param X A list of 2-column [tibble::tibble]s specifying a sample of
#'   persistence diagrams.
#' @param y An integer vector specifying persistence diagram labels (unused for
#'   now).
#'
#' @author Mathieu Carrière
#' @keywords internal
PreprocessingStep <- R6::R6Class(
  classname = "PreprocessingStep",
  inherit = SKLearnClass,
  public = list(
    #' @description Applies the class on a single persistence diagram and
    #'   outputs the result.
    #'
    #' @return A 2-column [tibble::tibble] storing the preprocessed persistence
    #'   diagram.
    apply = function(diag) {
      diag |>
        as.matrix() |>
        super$apply() |>
        private$convert_output()
    },

    #' @description Fits the class on a sample of persistence diagrams.
    #'
    #' @return The class itself invisibly.
    fit = function(X, y = NULL) {
      X |>
        purrr::map(as.matrix) |>
        super$fit(y)
      invisible(self)
    },

    #' @description Applies the class on a sample of persistence diagrams.
    #'
    #' @return A list of 2-column [tibble::tibble]s storing the preprocessed
    #'   persistence diagrams.
    transform = function(X) {
      X |>
        purrr::map(as.matrix) |>
        super$transform() |>
        purrr::map(private$convert_output)
    },

    #' @description Applies sequentially the `$fit()` and `$transform()` methods
    #'   on a sample of persistence diagrams in a more efficient way than
    #'   calling them directly.
    #'
    #' @return A list of 2-column [tibble::tibble]s storing the preprocessed
    #'   persistence diagrams.
    fit_transform = function(X, y = NULL) {
      X <- purrr::map(X, as.matrix)
      super$fit(X, y)
      X |>
        super$transform() |>
        purrr::map(private$convert_output)
    }
  ),
  private = list(
    variable_names = NULL,
    convert_output = function(x) {
      x |>
        `colnames<-`(value = private$variable_names) |>
        tibble::as_tibble()
    }
  )
)

#' Preprocessing: Birth Persistence Transform
#'
#' @description This is a class for the affine transformation \eqn{(x,y) \mapsto
#'   (x,y-x)} to be applied on persistence diagrams.
#'
#' @author Mathieu Carrière
#' @export
BirthPersistenceTransform <- R6::R6Class(
  classname = "BirthPersistenceTransform",
  inherit = PreprocessingStep,
  public = list(
    #' @description The [`BirthPersistenceTransform`] constructor.
    #'
    #' @return An object of class [`BirthPersistenceTransform`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #'   bpt <- BirthPersistenceTransform$new()
    #'   bpt$apply(dgm)
    #'   bpt$fit_transform(list(dgm))
    initialize = function() {
      private$variable_names <- c("birth", "lifetime")
      super$set_python_class(
        gd$representations$BirthPersistenceTransform()
      )
    }
  )
)

#' Preprocessing: Diagram Scaler
#'
#' @description This is a class for preprocessing persistence diagrams with a
#'   given list of scalers, such as those included in **scikit-learn**.
#'
#' @author Mathieu Carrière
#' @export
DiagramScaler <- R6::R6Class(
  classname = "DiagramScaler",
  inherit = PreprocessingStep,
  public = list(
    #' @description The [`DiagramScaler`] constructor.
    #'
    #' @param use A boolean value specifying whether to use the class. Defaults
    #'   to `FALSE`.
    #' @param scalers A list of scalers to be fit on the persistence diagrams.
    #'   Defaults to `list()` which is an empty list. Each element of the list
    #'   is a length-2 [base::list] with two elements:
    #'   - the first one is a list of coordinates;
    #'   - the second one is an instantiated scaler class. Choices are
    #'   [MaxAbsScaler], [MinMaxScaler], [RobustScaler] or [StandardScaler].
    #'
    #' @return An object of class [`DiagramScaler`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramScaler$new()
    #' ds$apply(dgm)
    #' ds$fit_transform(list(dgm))
    initialize = function(use = FALSE, scalers = list()) {
      private$variable_names <- c("birth", "death")
      super$set_python_class(
        gd$representations$DiagramScaler(use = use, scalers = scalers)
      )
    }
  )
)

#' Preprocessing: Diagram Selector
#'
#' @description This is a class for extracting finite or essential points in
#'   persistence diagrams.
#'
#' @author Mathieu Carrière
#' @export
DiagramSelector <- R6::R6Class(
  classname = "DiagramSelector",
  inherit = PreprocessingStep,
  public = list(
    #' @description The [`DiagramSelector`] constructor.
    #'
    #' @param use A boolean value specifying whether to use the class. Defaults
    #'   to `FALSE`.
    #' @param limit A numeric value specifying the second coordinate value which
    #'   is the criterion for being an essential point. Defaults to
    #'   \eqn{\infty}.
    #' @param point_type A string specifying the type of the points that are
    #'   going to be extracted. Choices are either `“finite”` or `“essential”`.
    #'   Defaults to `“finite”`.
    #'
    #' @return An object of class [`DiagramSelector`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new()
    #' ds$apply(dgm)
    #' ds$fit_transform(list(dgm))
    initialize = function(use = FALSE, limit = Inf, point_type = c("finite", "essential")) {
      point_type <- rlang::arg_match(point_type)
      private$variable_names <- c("birth", "death")
      super$set_python_class(
        gd$representations$DiagramSelector(
          use = use,
          limit = limit,
          point_type = point_type
        )
      )
    }
  )
)

#' Preprocessing: Padding
#'
#' @description This is a class for padding a list of persistence diagrams with
#'   dummy points, so that all persistence diagrams end up with the same number
#'   of points.
#'
#' @author Mathieu Carrière
#' @export
Padding <- R6::R6Class(
  classname = "Padding",
  inherit = PreprocessingStep,
  public = list(
    #' @description The [`Padding`] constructor.
    #'
    #' @param use A boolean value specifying whether to use the class. Defaults
    #'   to `FALSE`.
    #'
    #' @return An object of class [`Padding`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' pad <- Padding$new()
    #' pad$apply(dgm)
    #' pad$fit_transform(list(dgm))
    initialize = function(use = FALSE) {
      private$variable_names <- if (use) c("birth", "death", "original") else c("birth", "death")
      super$set_python_class(
        gd$representations$Padding(use = use)
      )
    }
  )
)

#' Preprocessing: Prominent Points
#'
#' @description This is a class or removing points that are close or far from
#'   the diagonal in persistence diagrams. If persistence diagrams are 2-column
#'   [tibble::tibble]s (i.e. persistence diagrams with ordinary features),
#'   points are ordered and thresholded by distance-to-diagonal. If persistence
#'   diagrams are 1-column [tibble::tibble]s (i.e. persistence diagrams with
#'   essential features), points are not ordered and thresholded by first
#'   coordinate.
#'
#' @author Mathieu Carrière
#' @export
ProminentPoints <- R6::R6Class(
  classname = "ProminentPoints",
  inherit = PreprocessingStep,
  public = list(
    #' @description The [`ProminentPoints`] constructor.
    #'
    #' @param use A boolean value specifying whether to use the class. Defaults
    #'   to `FALSE`.
    #' @param num_pts An integer value specifying the cardinality threshold.
    #'   Defaults to `10L`. If `location == "upper"`, keeps the top `num_pts`
    #'   points that are the farthest away from the diagonal. If `location ==
    #'   "lower"`, keeps the top `num_pts` points that are the closest to the
    #'   diagonal.
    #' @param threshold A numeric value specifying the distance-to-diagonal
    #'   threshold. Defaults to `-1.0`. If `location == "upper"`, keeps the
    #'   points that are at least at a distance threshold from the diagonal. If
    #'   `location == "lower"`, keeps the points that are at most at a distance
    #'   threshold from the diagonal.
    #' @param location A string specifying whether to keep the points that are
    #'   far away (`"upper"`) or close (`"lower"`) to the diagonal. Defaults to
    #'   `"upper"`.
    #'
    #' @return An object of class [`ProminentPoints`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' pp <- ProminentPoints$new()
    #' pp$apply(dgm)
    #' pp$fit_transform(list(dgm))
    initialize = function(use = FALSE, num_pts = 10, threshold = - 1, location = c("upper", "lower")) {
      num_pts <- as.integer(num_pts)
      location <- rlang::arg_match(location)
      private$variable_names <- c("birth", "death")
      super$set_python_class(
        gd$representations$ProminentPoints(
          use = use,
          num_pts = num_pts,
          threshold = threshold,
          location = location
        )
      )
    }
  )
)
