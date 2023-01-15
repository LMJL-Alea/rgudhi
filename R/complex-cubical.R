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
#' @author Pawel Dlotko
#' @family data structures for cell complexes
#'
#' @export
CubicalComplex <- R6::R6Class(
  classname = "CubicalComplex",
  inherit = PythonClass,
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
    #'   `NULL` which uses the Python class constructor instead.
    #'
    #' @return A new \code{\link{CubicalComplex}} object.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc
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
              super$set_python_class(
                gd$CubicalComplex(
                  top_dimensional_cells = top_dimensional_cells
                )
              )
            } else {
              super$set_python_class(
                gd$CubicalComplex(
                  dimensions = dimensions,
                  top_dimensional_cells = top_dimensional_cells
                )
              )
            }
          },
          perseus_file = {
            super$set_python_class(
              gd$CubicalComplex(
                perseus_file = perseus_file
              )
            )
          }
        )
      }
      else
        super$set_python_class(py_class)
    },

    #' @description This function returns the Betti numbers of the complex.
    #'
    #' @details The `$betti_numbers()` method always returns `[1, 0, 0, ...]` as
    #'   infinity filtration cubes are not removed from the complex.
    #'
    #' @return An integer vector storing the Betti numbers.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$compute_persistence()$betti_numbers()
    betti_numbers = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      super$get_python_class()$betti_numbers()
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
    #'   each row in each array correspond to: `(index of positive
    #'   top-dimensional cell, index of negative top-dimensional cell)`. The
    #'   second list contains the essential features, grouped by dimension. It
    #'   contains numpy arrays of shape `[number_of_persistence_points, 1]`. The
    #'   indices of the arrays in the list correspond to the homological
    #'   dimensions, and the integers of each row in each array correspond to:
    #'   `(index of positive top-dimensional cell)`.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$compute_persistence()$cofaces_of_persistence_pairs()
    cofaces_of_persistence_pairs = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      super$get_python_class()$cofaces_of_persistence_pairs()
    },

    #' @description This method computes the persistence of the complex, so it
    #'   can be accessed through `$persistent_betti_numbers()`,
    #'   `$persistence_intervals_in_dimension()`, etc. It is equivalent to the
    #'   `$persistence()` method when you do not want the list `$persistence()`
    #'   returns.
    #'
    #' @return The updated \code{\link{CubicalComplex}} class itself invisibly.
    compute_persistence = function(homology_coeff_field = 11,
                                   min_persistence = 0.0) {
      super$get_python_class()$compute_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence
      )
      private$m_ComputedPersistence <- TRUE
      invisible(self)
    },

    #' @description This function returns the dimension of the complex.
    #'
    #' @return An integer value giving the complex dimension.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$dimension()
    dimension = function() {
      super$get_python_class()$dimension()
    },

    #' @description This function returns the number of all cubes in the
    #'   complex.
    #'
    #' @return An integer value giving the number of all cubes in the complex.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$num_simplices()
    num_simplices = function() {
      super$get_python_class()$num_simplices()
    },

    #' @description This function computes and returns the persistence of the
    #'   complex.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing all persistence feature
    #'   summarised by 3 variables: `dimension`, `birth` and `death`.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$persistence()
    persistence = function(homology_coeff_field = 11,
                           min_persistence = 0.0) {
      l <- super$get_python_class()$persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence
      )
      l <- purrr::map(l, purrr::simplify_all)
      l <- purrr::map(l, rlang::set_names, nm = c("dimension", "barcode"))

      l_dim <- purrr::map(l, "dimension")
      l_dim <- purrr::map(l_dim, rlang::set_names, nm = "dimension")
      l_dim <- purrr::transpose(l_dim)
      l_dim <- purrr::simplify_all(l_dim)
      l_dim <- tibble::as_tibble(l_dim)

      l_bar <- purrr::map(l, "barcode")
      l_bar <- purrr::map(l_bar, rlang::set_names, nm = c("birth", "death"))
      l_bar <- purrr::transpose(l_bar)
      l_bar <- purrr::simplify_all(l_bar)
      l_bar <- tibble::as_tibble(l_bar)

      tibble::tibble(l_dim, l_bar)
    },

    #' @description This function returns the persistence intervals of the
    #'   complex in a specific dimension.
    #'
    #' @param dimension An integer value specifying the desired dimension.
    #'
    #' @return A \code{\link[tibble]{tibble}} storing the persistence intervals
    #'   by row.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$compute_persistence()$persistence_intervals_in_dimension(0)
    persistence_intervals_in_dimension = function(dimension) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      M <- super$get_python_class()$persistence_intervals_in_dimension(dimension)
      colnames(M) <- c("birth", "death")
      tibble::as_tibble(M)
    },

    #' @description This function returns the persistent Betti numbers of the
    #'   complex.
    #'
    #' @param from_value A numeric value specifying the persistence birth limit
    #'   to be added in the numbers (`persistent birth <= from_value`).
    #' @param to_value A numeric value specifying the persistence death limit to
    #'   be added in the numbers (`persistent death > to_value`).
    #'
    #' @return An integer vector storing the persistent Betti numbers.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' cc <- CubicalComplex$new(top_dimensional_cells = X)
    #' cc$compute_persistence()$persistent_betti_numbers(0, 1)
    persistent_betti_numbers = function(from_value, to_value) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      super$get_python_class()$persistent_betti_numbers(
        from_value = from_value,
        to_value = to_value
      )
    }
  ),
  private = list(
    m_ComputedPersistence = FALSE
  )
)

#' R6 Class for Periodic Cubical Complex
#'
#' @description The `PeriodicCubicalComplex` class is an example of a structured
#'   complex useful in computational mathematics (specially rigorous numerics)
#'   and image analysis.
#'
#' @author Pawel Dlotko
#' @family data structures for cell complexes
#'
#' @export
PeriodicCubicalComplex <- R6::R6Class(
  classname = "PeriodicCubicalComplex",
  inherit = CubicalComplex,
  public = list(
    #' @description Constructor from either `top_dimensional_cells` (and
    #'   possibly `dimensions`) or from a Perseus-style file name.
    #'
    #' @param perseus_file A character string specifying the path to a
    #'   Perseus-style file name.
    #' @param top_dimensional_cells Either a numeric vector (in which case,
    #'   `dimensions` should be provided as well) or a multidimensional array
    #'   specifying cell filtration values.
    #' @param periodic_dimensions A logical vector specifying the periodicity
    #'   value of the top dimensional cells.
    #' @param dimensions An integer vector specifying the number of top
    #'   dimensional cells. Defaults to `NULL`.
    #' @param py_class An existing `PeriodicCubicalComplex` Python class.
    #'   Defaults to `NULL` which uses the Python class constructor instead.
    #'
    #' @return A new \code{\link{PeriodicCubicalComplex}} object.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' n <- 10
    #' X <- cbind(seq(0, 1, len = n), seq(0, 1, len = n))
    #' pcc <- PeriodicCubicalComplex$new(
    #'   top_dimensional_cells = X,
    #'   periodic_dimensions = c(TRUE, FALSE)
    #' )
    #' pcc
    initialize = function(perseus_file,
                          top_dimensional_cells,
                          periodic_dimensions,
                          dimensions = NULL,
                          py_class = NULL) {
      if (rlang::is_null(py_class)) {
        switch(
          rlang::check_exclusive(top_dimensional_cells, perseus_file),
          top_dimensional_cells = {
            rlang::check_required(periodic_dimensions)
            dims <- dim(top_dimensional_cells)
            if (length(periodic_dimensions) == 1)
              periodic_dimensions <- list(periodic_dimensions)
            if (!rlang::is_null(dims)) {
              super$set_python_class(
                gd$PeriodicCubicalComplex(
                  top_dimensional_cells = top_dimensional_cells,
                  periodic_dimensions = periodic_dimensions
                )
              )
            } else {
              super$set_python_class(
                gd$PeriodicCubicalComplex(
                  dimensions = dimensions,
                  top_dimensional_cells = top_dimensional_cells,
                  periodic_dimensions = periodic_dimensions
                )
              )
            }
          },
          perseus_file = {
            super$set_python_class(
              gd$PeriodicCubicalComplex(
                perseus_file = perseus_file
              )
            )
          }
        )
      }
      else
        super$set_python_class(py_class)
    }
  )
)
