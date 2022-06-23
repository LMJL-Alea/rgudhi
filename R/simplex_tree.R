#' R6 Class for Simplex Tree
#'
#' @description The simplex tree is an efficient and flexible data structure for
#'   representing general (filtered) simplicial complexes. The data structure is
#'   described in Jean-Daniel Boissonnat and Clément Maria. The Simplex Tree: An
#'   Efficient Data Structure for General Simplicial Complexes. Algorithmica,
#'   pages 1–22, 2014.
#'
#' @details This class is a filtered, with keys, and non contiguous vertices
#'   version of the simplex tree.
#'
#' @export
SimplexTree <- R6::R6Class(
  classname = "SimplexTree",
  public = list(
    #' @description `SimplexTree` constructor.
    #'
    #' @param py_class A Python `SimplexTree` class object. Defaults to `NULL`
    #'   which uses the Python class constructor instead.
    #'
    #' @return A new `SimplexTree` object.
    #'
    #' @examples
    #' st <- SimplexTree$new()
    initialize = function(py_class = NULL) {
      if (is.null(py_class))
        private$py_class <- gd$SimplexTree()
      else
        private$py_class <- py_class
    },

    #' @description This function assigns a new filtration value to a given
    #'   N-simplex.
    #'
    #' @details Beware that after this operation, the structure may not be a
    #'   valid filtration anymore, a simplex could have a lower filtration value
    #'   than one of its faces. Callers are responsible for fixing this (with
    #'   more `assign_filtration()` or `make_filtration_non_decreasing()` for
    #'   instance) before calling any function that relies on the filtration
    #'   property, like `persistence()`.
    #'
    #' @param simplex A integer vector representing the N-simplex in the form
    #'   of a list of vertices.
    #' @param filtration A numeric value specifying the new filtration value
    assign_filtration = function(simplex, filtration) {
      private$py_class$assign_filtration(
        simplex = simplex,
        filtration = filtration
      )
    },

    #' @description This function returns the Betti numbers of the simplicial
    #'   complex.
    #'
    #' @return An integer vector storing the Betti numbers.
    betti_numbers = function() {
      if (!private$is_persistence_computed)
        self$compute_persistence()
      private$py_class$betti_numbers()
    },

    #' @description Assuming the simplex tree is a 1-skeleton graph, this method
    #'   collapse edges (simplices of higher dimension are ignored) and resets
    #'   the simplex tree from the remaining edges. A good candidate is to build
    #'   a simplex tree on top of a RipsComplex of dimension 1 before collapsing
    #'   edges (cf. rips_complex_edge_collapse_example.py). For implementation
    #'   details, please refer to [6].
    #'
    #' @details It requires `Eigen >= 3.1.0` and an exception is thrown if not
    #'   available.
    #'
    #' @param nb_iterations An integer value specifying the number of edge
    #'   collapse iterations to perform. Defaults to `1L`.
    collapse_edges = function(nb_iterations = 1) {
      private$py_class$collapse_edges(nb_iterations = nb_iterations)
    },

    #' @description This function computes the persistence of the simplicial
    #'   complex, so it can be accessed through `persistent_betti_numbers()`,
    #'   `persistence_pairs()`, etc. This function is equivalent to
    #'   `persistence()` when you do not want the list that `persistence()`
    #'   returns.
    #'
    #' @param homology_coeff_field An integer value specifying the homology
    #'   coefficient field. Must be a prime number. Defaults to `11L`. Maximum
    #'   is `46337L`.
    #' @param min_persistence A numeric value specifying the minimum persistence
    #'   value to take into account (strictly greater than `min_persistence`).
    #'   Defaults to `0.0`. Set `min_persistence = -1.0` to see all values.
    #' @param persistence_dim_max A boolean specifying whether the persistent
    #'   homology for the maximal dimension in the complex is computed
    #'   (`persistence_dim_max = TRUE`). If `FALSE`, it is ignored. Defaults to
    #'   `FALSE`.
    compute_persistence = function(homology_coeff_field = 11,
                                   min_persistence = 0.0,
                                   persistence_dim_max = FALSE) {
      private$py_class$compute_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence,
        persistence_dim_max = persistence_dim_max
      )
      private$is_persistence_computed = TRUE
    }
  ),
  private = list(
    py_class = NULL,
    is_persistence_computed = FALSE
  )
)
