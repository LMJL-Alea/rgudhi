#' R6 Class for Witness Complex
#'
#' @description \loadmathjax A Witness complex \mjseqn{\mathrm{Wit}(W,L)} is a
#'   simplicial complex defined on two sets of points in \mjseqn{\mathbb{R}^D}.
#'   The data structure is described in
#'   \insertCite{boissonnat2014simplex;textual}{rgudhi}.
#'
#' @details The class constructs a (weak) witness complex for a given table of
#'   nearest landmarks with respect to witnesses.
#'
#' ## References
#'
#' \insertAllCited{}
#'
#' @author Siargey Kachanovich
#'
#' @export
WitnessComplex <- R6::R6Class(
  classname = "WitnessComplex",
  public = list(
    #' @description `WitnessComplex` constructor.
    #'
    #' @param nearest_landmark_table A list of \code{\link[tibble]{tibble}}s
    #'   specifying for each *witness* `w`, the ordered list of nearest
    #'   landmarks with id in column `nearest_landmark` and distance to `w` in
    #'   column `distance`.
    #'
    #' @return A \code{\link{WitnessComplex}} object storing the Witness
    #'   complex.
    #'
    #' @examples
    #' set.seed(1234)
    #' l <- list(
    #'   tibble::tibble(
    #'     nearest_landmark = sample.int(10),
    #'     distance = sort(rexp(10))
    #'   ),
    #'   tibble::tibble(
    #'     nearest_landmark = sample.int(10),
    #'     distance = sort(rexp(10))
    #'   )
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   wc <- WitnessComplex$new(nearest_landmark_table = l)
    #'   wc
    #' }
    initialize = function(nearest_landmark_table) {
      if (!rlang::is_list(nearest_landmark_table))
        cli::cli_abort("The input should be a list.")
      for (t in nearest_landmark_table) {
        if (!tibble::is_tibble(t))
          cli::cli_abort("All elements of the input list should be tibbles.")
        if (!all(c("nearest_landmark", "distance") == names(t)))
          cli::cli_abort("All elements of the input list should be tibbles with exactly the two column {.code nearest_landmark} and {.code distance}.")
      }

      nearest_landmark_table <- purrr::map(
        .x = nearest_landmark_table,
        .f = purrr::array_tree,
        margin = 1
      )

      private$m_PythonClass <- gd$WitnessComplex(
        nearest_landmark_table = nearest_landmark_table
      )
    },

    #' @param max_alpha_square The maximum relaxation parameter. Defaults to
    #'   `Inf`.
    #'
    #' @return A \code{\link{SimplexTree}} object storing the computed simplex
    #'   tree created from the Delaunay triangulation.
    #'
    #' @examples
    #' set.seed(1234)
    #' l <- list(
    #'   tibble::tibble(
    #'     nearest_landmark = sample.int(10),
    #'     distance = sort(rexp(10))
    #'   ),
    #'   tibble::tibble(
    #'     nearest_landmark = sample.int(10),
    #'     distance = sort(rexp(10))
    #'   )
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   wc <- WitnessComplex$new(nearest_landmark_table = l)
    #'   st <- wc$create_simplex_tree()
    #'   st$num_vertices()
    #' }
    create_simplex_tree = function(max_alpha_square = Inf) {
      py_st <- private$m_PythonClass$create_simplex_tree(
        max_alpha_square = max_alpha_square
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

#' R6 Class for Strong Witness Complex
#'
#' @inherit WitnessComplex description
#' @details The class constructs a (strong) witness complex for a given table of
#'   nearest landmarks with respect to witnesses.
#'
#' @export
StrongWitnessComplex <- R6::R6Class(
  classname = "StrongWitnessComplex",
  inherit = WitnessComplex,
  public = list(
    #' @description `StrongWitnessComplex` constructor.
    #'
    #' @param nearest_landmark_table A list of \code{\link[tibble]{tibble}}s
    #'   specifying for each *witness* `w`, the ordered list of nearest
    #'   landmarks with id in column `nearest_landmark` and distance to `w` in
    #'   column `distance`.
    #'
    #' @return A \code{\link{StrongWitnessComplex}} object storing the strong
    #'   Witness complex.
    #'
    #' @examples
    #' set.seed(1234)
    #' l <- list(
    #'   tibble::tibble(
    #'     nearest_landmark = sample.int(10),
    #'     distance = sort(rexp(10))
    #'   ),
    #'   tibble::tibble(
    #'     nearest_landmark = sample.int(10),
    #'     distance = sort(rexp(10))
    #'   )
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   wc <- StrongWitnessComplex$new(nearest_landmark_table = l)
    #'   wc
    #' }
    initialize = function(nearest_landmark_table) {
      if (!rlang::is_list(nearest_landmark_table))
        cli::cli_abort("The input should be a list.")
      for (t in nearest_landmark_table) {
        if (!tibble::is_tibble(t))
          cli::cli_abort("All elements of the input list should be tibbles.")
        if (!all(c("nearest_landmark", "distance") == names(t)))
          cli::cli_abort("All elements of the input list should be tibbles with exactly the two column {.code nearest_landmark} and {.code distance}.")
      }

      nearest_landmark_table <- purrr::map(
        .x = nearest_landmark_table,
        .f = purrr::array_tree,
        margin = 1
      )

      private$m_PythonClass <- gd$StrongWitnessComplex(
        nearest_landmark_table = nearest_landmark_table
      )
    }
  )
)
