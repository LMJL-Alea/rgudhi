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
    #' @param data Either a `n x d` matrix or a length-`n` list of
    #'   `d`-dimensional vectors or a distance matrix stored as a
    #'   \code{\link[stats]{dist}} object.
    #' @param max_edge_length A numeric value specifying the Rips value.
    #' @param sparse A numeric value specifying the approximation parameter
    #'   epsilon for buidling a sparse Rips complex. Defaults to `NULL` which
    #'   builds an exact Rips complex.
    #'
    #' @return A \code{\link{RipsComplex}} object storing the Rips complex.
    #'
    #' @examples
    #' n <- 10
    #' Xl <- replicate(n, runif(2), simplify = FALSE)
    #' if (reticulate::py_module_available("gudhi")) {
    #'   rc1 <- RipsComplex$new(data = Xl, max_edge_length = 1)
    #' }
    #' Xm <- Reduce(rbind, Xl, init = numeric())
    #' if (reticulate::py_module_available("gudhi")) {
    #'   rc2 <- RipsComplex$new(data = Xm, max_edge_length = 1)
    #' }
    #' D <- dist(Xm)
    #' if (reticulate::py_module_available("gudhi")) {
    #'   rc3 <- RipsComplex$new(data = D)
    #' }
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
          distance_matrix = as.matrix(data),
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
    #' if (reticulate::py_module_available("gudhi")) {
    #'   rc <- RipsComplex$new(data = X, max_edge_length = 1)
    #'   st <- rc$create_simplex_tree(1)
    #' }
    create_simplex_tree = function(max_dimension) {
      py_st <- private$m_PythonClass$create_simplex_tree(
        max_dimension = max_dimension
      )
      private$m_ComputedSimplexTree <- TRUE
      st <- SimplexTree$new(py_class = py_st)
      st$set_is_flag(TRUE)
      st
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedSimplexTree = FALSE
  )
)