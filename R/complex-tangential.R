#' R6 Class for Tangential Complex
#'
#' @description A Tangential Delaunay complex is a simplicial complex designed
#'   to reconstruct a \eqn{k}-dimensional manifold embedded in
#'   \eqn{d}-dimensional Euclidean space. The input is a point sample coming
#'   from an unknown manifold. The running time depends only linearly on the
#'   extrinsic dimension \eqn{d} and exponentially on the intrinsic dimension
#'   \eqn{k}.
#'
#' @details The [TangentialComplex] class represents a tangential complex. After
#'   the computation of the complex, an optional post-processing called
#'   perturbation can be run to attempt to remove inconsistencies.
#'
#' @author Clément Jamin
#' @family filtrations and reconstructions
#'
#' @export
TangentialComplex <- R6::R6Class(
  classname = "TangentialComplex",
  inherit = PythonClass,
  public = list(
    #' @description `TangentialComplex` constructor.
    #'
    #' @param points Either a character string specifying the path to an OFF
    #'   file which the points can be read from or a numeric matrix or list of
    #'   numeric vectors specifying the points directly.
    #' @param intrinsic_dim An integer value specifying the intrinsic dimension
    #'   of the manifold. This is nedded when points are provided as a numeric
    #'   matrix or a list of numeric vectors. Defaults to `NULL`.
    #'
    #' @return A \code{\link{TangentialComplex}} object storing the tangential
    #'   complex.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc
    initialize = function(points, intrinsic_dim = NULL) {
      if (is.character(points) && fs::path_ext(points) == "off") {
        super$set_python_class(
          gd$TangentialComplex(
            off_file = points
          )
        )
      } else {
        if (is.null(intrinsic_dim))
          cli::cli_abort("When {.code points} is a numeric matrix or a list of numeric vectors, the intrinsic dimension of the manifold needs to be specified via the {.code intrinsic_dim} argument.")
        super$set_python_class(
          gd$TangentialComplex(
            points = points,
            intrisic_dim = intrinsic_dim
          )
        )
      }
    },

    #' @description This function computes the tangential complex.
    #'
    #' @details In debug mode, it may raise a `ValueError` if the computed star
    #'   dimension is too low. Try to set a bigger maximal edge length value via
    #'   the `$set_max_squared_edge_length()` method if this happens.
    #'
    #' @return The updated \code{\link{TangentialComplex}} class itself
    #'   invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc$compute_tangential_complex()
    compute_tangential_complex = function() {
      super$get_python_class()$compute_tangential_complex()
      private$m_ComputedTangentialComplex <- TRUE
      invisible(self)
    },

    #' @description Exports the complex into a simplex tree.
    #'
    #' @return A \code{\link{SimplexTree}} object storing the computed simplex
    #'   tree.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' st <- tc$compute_tangential_complex()$create_simplex_tree()
    create_simplex_tree = function() {
      if (!private$m_ComputedTangentialComplex)
        cli::cli_abort("You first need to compute the tangential complex by calling the {.code $compute_tangential_complex()} method.")
      py_st <- super$get_python_class()$create_simplex_tree()
      private$m_ComputedSimplexTree <- TRUE
      SimplexTree$new(py_class = py_st)
    },

    #' @description This function returns the point corresponding to a given
    #'   vertex from the \code{\link{SimplexTree}}.
    #'
    #' @param vertex An integer value specifying the desired vertex.
    #'
    #' @return A numeric vector storing the point corresponding to the input
    #'   vertex.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' st <- tc$compute_tangential_complex()$create_simplex_tree()
    #' tc$get_point(1)
    get_point = function(vertex) {
      if (!private$m_ComputedSimplexTree)
        cli::cli_abort("You first need to generate the simplex tree by calling the {.code $create_simplex_tree()} method.")
      super$get_python_class()$get_point(vertex)
    },

    #' @return An integer value storing the number of inconsistent simplicies.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc$compute_tangential_complex()
    #' tc$num_inconsistent_simplices()
    num_inconsistent_simplices = function() {
      super$get_python_class()$num_inconsistent_simplices()
    },

    #' @return An integer value storing the number of stars containing at least
    #'   one inconsistent simplex.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc$compute_tangential_complex()
    #' tc$num_inconsistent_stars()
    num_inconsistent_stars = function() {
      super$get_python_class()$num_inconsistent_stars()
    },

    #' @return An integer value storing the total number of simplices in stars
    #'   (including duplicates that appear in several stars).
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc$compute_tangential_complex()
    #' tc$num_simplices()
    num_simplices = function() {
      super$get_python_class()$num_simplices()
    },

    #' @return An integer value storing the number of vertices.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc$compute_tangential_complex()
    #' tc$num_vertices()
    num_vertices = function() {
      super$get_python_class()$num_vertices()
    },

    #' @description Sets the maximal possible squared edge length for the edges
    #'   in the triangulations.
    #'
    #' @details If the maximal edge length value is too low, the
    #'   `$compute_tangential_complex()` method will throw an exception in debug
    #'   mode.
    #'
    #' @param max_squared_edge_length A numeric value specifying the maximal
    #'   possible squared edge length.
    #'
    #' @return The updated \code{\link{TangentialComplex}} class itself
    #'   invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' tc <- TangentialComplex$new(points = X, intrinsic_dim = 1)
    #' tc$set_max_squared_edge_length(1)
    set_max_squared_edge_length = function(max_squared_edge_length) {
      super$get_python_class()$set_max_squared_edge_length(max_squared_edge_length)
      invisible(self)
    }
  ),
  private = list(
    m_ComputedTangentialComplex = FALSE,
    m_ComputedSimplexTree = FALSE
  )
)
