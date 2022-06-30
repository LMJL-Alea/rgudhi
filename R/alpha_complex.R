#' R6 Class for Alpha Complex
#'
#' @description  AlphaComplex is a simplicial complex constructed from the
#'   finite cells of a Delaunay Triangulation.
#'
#' @details The filtration value of each simplex is computed as the square of the
#' circumradius of the simplex if the circumsphere is empty (the simplex is then
#' said to be Gabriel), and as the minimum of the filtration values of the
#' codimension 1 cofaces that make it not Gabriel otherwise. All simplices that
#' have a filtration value strictly greater than a given alpha squared value are
#' not inserted into the complex.
#'
#' @export
AlphaComplex <- R6::R6Class(
  classname = "AlphaComplex",
  public = list(
    #' @description `AlphaComplex` constructor.
    #'
    #' @param points Either a `n x d` matrix or a length-`n` list of
    #'   `d`-dimensional vectors or a file with extension `.off`.
    #' @param precision A string specifying the alpha complex precision. Can be
    #'   one of `"fast"`, `"safe"` or `"exact"`. Defaults to `"safe"`.
    #'
    #' @return A \code{\link{AlphaComplex}} object storing the Alpha complex.
    #'
    #' @examples
    #' n <- 10
    #' X_list <- replicate(n, runif(2), simplify = FALSE)
    #' X_matrix <- Reduce(rbind, X_list, init = numeric())
    #' ac_matrix <- AlphaComplex$new(points = X_matrix)
    #' ac_list <- AlphaComplex$new(points = X_list)
    initialize = function(points, precision = "safe") {
      if (inherits(points, "matrix") || inherits(points, "list"))
        private$m_PythonClass <- gd$AlphaComplex(points = points)
      else if (is.character(points) && fs::path_ext(points) == "off")
        private$m_PythonClass <- gd$AlphaComplex(off_file = points, precision = precision)
      else
        cli::cli_abort("{.code points} must be either a matrix or a list or an OFF file.")
    },

    #' @description Generates a simplex tree from the Delaunay triangulation.
    #'
    #' @param max_alpha_square A numeric value specifying the maximum alpha
    #'   square threshold the simplices shall not exceed. Default is set to
    #'   `Inf`, and there is very little point using anything else since it does
    #'   not save time.
    #' @param default_filtration_value A boolean specifying whether filtration
    #'   values should not be computed and will be set to `NaN`
    #'   (`default_filtration_value = TRUE`). Defaults to `FALSE` (which means
    #'   compute the filtration values).
    #'
    #' @return A \code{\link{SimplexTree}} object storing the computed simplex
    #'   tree.
    #'
    #' @examples
    #' n <- 10
    #' X <- replicate(n, runif(2), simplify = FALSE)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    create_simplex_tree = function(max_alpha_square = Inf,
                                   default_filtration_value = FALSE) {
      py_st <- private$m_PythonClass$create_simplex_tree(
        max_alpha_square = max_alpha_square,
        default_filtration_value = default_filtration_value
      )
      private$m_ComputedSimplexTree <- TRUE
      SimplexTree$new(py_class = py_st)
    },

    #' @description This function returns the point corresponding to a given
    #'   vertex from the SimplexTree.
    #'
    #' @param vertex An integer value specifying the desired vertex.
    #'
    #' @return A numeric vector storing the point corresponding to the input
    #'   vertex.
    #'
    #' @examples
    #' n <- 10
    #' X <- replicate(n, runif(2), simplify = FALSE)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' ac$get_point(1)
    get_point = function(vertex) {
      if (!private$m_ComputedSimplexTree)
        cli::cli_abort("You first need to generate the simplex tree by calling the {.code $create_simplex_tree()} method.")
      private$m_PythonClass$get_point(vertex)
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedSimplexTree = FALSE
  )
)
