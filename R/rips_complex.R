#' R6 Class for Rips Complex
#'
#' @description The data structure is a one skeleton graph, or Rips graph,
#'   containing edges when the edge length is less or equal to a given
#'   threshold. Edge length is computed from a user given point cloud with a
#'   given distance function, or a distance matrix.
#'
#' @export
RipsComplex <- R6::R6Class(
  classname = "RipsComplex",
  public = list(
    #' @description `RipsComplex` constructor.
    #'
    #' @param points Either a `n x d` matrix or a length-`n` list of
    #'   `d`-dimensional vectors or a file with extension `.off`.
    #' @param precision A string specifying the alpha complex precision. Can be
    #'   one of `"fast"`, `"safe"` or `"exact"`. Defaults to `"safe"`.
    #'
    #' @return A \code{\link{RipsComplex}} object storing the Rips complex.
    #'
    #' @examples
    #' n <- 10
    #' X_list <- replicate(n, runif(2), simplify = FALSE)
    #' X_matrix <- Reduce(rbind, X_list, init = numeric())
    #' rc_matrix <- RipsComplex$new(points = X_matrix)
    #' rc_list <- RipsComplex$new(points = X_list)
    initialize = function(data, max_edge_length = NULL, sparse = NULL) {
      if (inherits(data, "matrix") || inherits(data, "list")) {
        if (is.null(max_edge_length))
          cli::cli_abort("You need to provide a value for the {.code max_edge_length} argument when using points as data input.")
        private$m_PythonClass <- gd$RipsComplex(
          points = data,
          max_edge_length = max_edge_length,
          sparse = sparse
        )
      } else if (inherits(data, "dist"))
        private$m_PythonClass <- gd$RipsComplex(
          distance_matrix = data,
          sparse = sparse
        )
      else
        cli::cli_abort("{.code data} must be either a {.code matrix} or a {.code list} or a {.code dist} file.")
    },

    #' @param max_dimension An integer value specifying the maximal dimension
    #'   which the Rips complex will be expanded to.
    #'
    #' @return A \code{\link{SimplexTree}} object storing the computed simplex
    #'   tree.
    #'
    #' @examples
    #' n <- 10
    #' X <- replicate(n, runif(2), simplify = FALSE)
    #' rc <- RipsComplex$new(points = X)
    #' st <- rc$create_simplex_tree()
    create_simplex_tree = function(max_dimension) {
      py_st <- private$m_PythonClass$create_simplex_tree(
        max_dimension = max_dimension
      )
      private$m_ComputedSimplexTree <- TRUE
      SimplexTree$new(py_class = py_st)
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedSimplexTree = FALSE
  )
)
