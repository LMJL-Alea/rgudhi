#' R6 Class for Cubical Complex
#'
#' @description The CubicalComplex is an example of a structured complex useful
#'   in computational mathematics (specially rigorous numerics) and image
#'   analysis.
#'
#' @param homology_coeff_field An integer value specifying the homology
#'   coefficient field. Must be a prime number. Defaults to `11L`. Maximum
#'   is `46337L`.
#' @param min_persistence A numeric value specifying the minimum persistence
#'   value to take into account (strictly greater than `min_persistence`).
#'   Defaults to `0.0`. Set `min_persistence = -1.0` to see all values.
#'
#' @export
CubicalComplex <- R6::R6Class(
  classname = "CubicalComplex",
  public = list(
    #' @description Constructor from either `top_dimensional_cells` (and
    #'   possibly `dimensions`) or from a Perseus-style file name.
    #'
    #' @param perseus_file A character string specifying the path to a
    #'   Perseus-style file name.
    #' @param top_dimensional_cells Either a numeric vector (in which case,
    #'   `dimensions` should be provided as well) or a multidimensional array
    #'   specifying cell filtration values.
    #' @param dimensions An integer vector specifying the number of top
    #'   dimensional cells. Defaults to `NULL`.
    #' @param py_class An existing `CubicalComplex` Python class. Defaults to
    #'   `NULL`.
    #'
    #' @return A new \code{\link{CubicalComplex}} object.
    #'
    #' @examples
    #' X <- matrix(rnorm(20), nrow = 10)
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    initialize = function(perseus_file,
                          top_dimensional_cells,
                          dimensions = NULL,
                          py_class = NULL) {
      if (rlang::is_null(py_class)) {
        switch(
          rlang::check_exclusive(top_dimensional_cells, perseus_file),
          top_dimensional_cells = {
            dims <- dim(top_dimensional_cells)
            if (!rlang::is_null(dims)) {
              private$m_PythonClass <- gd$CubicalComplex(
                top_dimensional_cells = top_dimensional_cells
              )
            } else {
              private$m_PythonClass <- gd$CubicalComplex(
                dimensions = dimensions,
                top_dimensional_cells = top_dimensional_cells
              )
            }
          },
          perseus_file = {
            private$m_PythonClass <- gd$CubicalComplex(
              perseus_file = perseus_file
            )
          }
        )
      }
      else
        private$m_PythonClass <- py_class
    },

    #' @description This function returns the Betti numbers of the complex.
    #'
    #' @details The `$betti_numbers()` method always returns `[1, 0, 0, ...]` as
    #'   infinity filtration cubes are not removed from the complex.
    #'
    #' @return An integer vector storing the Betti numbers.
    betti_numbers = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$betti_numbers()
    },

    #' @description A persistence interval is described by a pair of cells, one
    #'   that creates the feature and one that kills it. The filtration values
    #'   of those 2 cells give coordinates for a point in a persistence diagram,
    #'   or a bar in a barcode. Structurally, in the cubical complexes provided
    #'   here, the filtration value of any cell is the minimum of the filtration
    #'   values of the maximal cells that contain it. Connecting persistence
    #'   diagram coordinates to the corresponding value in the input (i.e. the
    #'   filtration values of the top-dimensional cells) is useful for
    #'   differentiation purposes.
    #'
    #' @details This function returns a list of pairs of top-dimensional cells
    #'   corresponding to the persistence birth and death cells of the
    #'   filtration. The cells are represented by their indices in the input
    #'   list of top-dimensional cells (and not their indices in the internal
    #'   data structure that includes non-maximal cells). Note that when two
    #'   adjacent top-dimensional cells have the same filtration value, we
    #'   arbitrarily return one of the two when calling the function on one of
    #'   their common faces.
    #'
    #' @return The top-dimensional cells/cofaces of the positive and negative
    #'   cells, together with the corresponding homological dimension, in two
    #'   lists of integer arrays. The first list contains the regular
    #'   persistence pairs, grouped by dimension. It contains numpy arrays of
    #'   shape `[number_of_persistence_points, 2]`. The indices of the arrays in
    #'   the list correspond to the homological dimensions, and the integers of
    #'   each row in each array correspond to: (index of positive
    #'   top-dimensional cell, index of negative top-dimensional cell). The
    #'   second list contains the essential features, grouped by dimension. It
    #'   contains numpy arrays of shape `[number_of_persistence_points, 1]`. The
    #'   indices of the arrays in the list correspond to the homological
    #'   dimensions, and the integers of each row in each array correspond to:
    #'   (index of positive top-dimensional cell).
    cofaces_of_persistence_pairs = function() {
      private$m_PythonClass$cofaces_of_persistence_pairs()
    },

    #' @description This method computes the persistence of the complex, so it
    #'   can be accessed through `$persistent_betti_numbers()`,
    #'   `$persistence_intervals_in_dimension()`, etc. It is equivalent to the
    #'   `$persistence()` method when you do not want the list `$persistence()`
    #'   returns.
    compute_persistence = function(homology_coeff_field = 11,
                                   min_persistence = 0.0) {
      private$m_PythonClass$compute_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence
      )
    },

    #' @description This function returns the dimension of the complex.
    #'
    #' @return An integer value giving the complex dimension.
    #'
    #' @examples
    #' X <- matrix(rnorm(20), nrow = 10)
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$dimension()
    dimension = function() {
      private$m_PythonClass$dimension()
    },

    #' @description This function returns the number of all cubes in the
    #'   complex.
    #'
    #' @return An integer value giving the number of all cubes in the complex.
    #'
    #' @examples
    #' X <- matrix(rnorm(20), nrow = 10)
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$num_simplices()
    num_simplices = function() {
      private$m_PythonClass$num_simplices()
    },

    #' @description This function computes and returns the persistence of the
    #'   complex.
    #'
    #' @return A list of length-2 lists with components:
    #'
    #' - `dimension`: An integer value storing the dimension;
    #' - `barcode`: A length-2 numeric vector storing the birth and death.
    #'
    #' @examples
    #' X <- matrix(rnorm(20), nrow = 10)
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$persistence()
    persistence = function(homology_coeff_field = 11,
                           min_persistence = 0.0) {
      l <- private$m_PythonClass$persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence
      )
      l <- purrr::map(l, purrr::simplify_all)
      l <- purrr::map(l, purrr::set_names, nm = c("dimension", "barcode"))
      l
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedPersistence = FALSE
  )
)
