#' R6 Class for Simplex Tree
#'
#' @description The simplex tree is an efficient and flexible data structure for
#'   representing general (filtered) simplicial complexes. The data structure is
#'   described in \insertCite{boissonnat2014simplex;textual}{rgudhi}.
#'
#' @details This class is a filtered, with keys, and non contiguous vertices
#'   version of the simplex tree.
#'
#' ## References
#'
#' \insertCited{}
#'
#' @param simplex An integer vector representing the N-simplex in the form
#'   of a list of vertices.
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
#' @param chainable A boolean specifying whether the method should return the
#'   class itself, hence allowing its use in pipe chaining. Defaults to `TRUE`,
#'   which enables chaining.
#'
#' @author Clément Maria
#' @family data structures for cell complexes
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
    #' @return A new \code{\link{SimplexTree}} object.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   st <- SimplexTree$new()
    #'   st
    #' }
    initialize = function(py_class = NULL) {
      if (is.null(py_class))
        private$m_PythonClass <- gd$SimplexTree()
      else
        private$m_PythonClass <- py_class
    },

    #' @description This function sets the internal field `m_IsFlag` which
    #'   records whether the simplex tree is a flag complex (i.e. has been
    #'   generated by a Rips complex).
    #'
    #' @details The \code{\link{SimplexTree}} class initializes the `m_IsFlag`
    #'   field to `FALSE` by default and this method specifically allows to
    #'   overwrite this default value.
    #'
    #' @param val A boolean specifying whether the simplex tree is a flag
    #'   complex.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$set_is_flag(TRUE)
    #' }
    set_is_flag = function(val) {
      private$m_IsFlag <- val
      invisible(self)
    },

    #' @description This function assigns a new filtration value to a given
    #'   N-simplex.
    #'
    #' @details Beware that after this operation, the structure may not be a
    #'   valid filtration anymore, a simplex could have a lower filtration value
    #'   than one of its faces. Callers are responsible for fixing this (with
    #'   more calls to the `$assign_filtration()` method or a call to the
    #'   `$make_filtration_non_decreasing()` method for instance) before calling
    #'   any function that relies on the filtration property, such as
    #'   `persistence()`.
    #'
    #' @param filtration A numeric value specifying the new filtration value.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$filtration(1)
    #'   st$assign_filtration(1, 0.8)
    #'   st$filtration(1)
    #' }
    assign_filtration = function(simplex, filtration) {
      if (!self$find(simplex)) {
        cli::cli_alert_warning("The input simplex {simplex} is not currently included in the simplex tree. Nothing to do.")
        return()
      }
      if (length(simplex) == 1)
        simplex <- list(simplex)
      private$m_PythonClass$assign_filtration(
        simplex = simplex,
        filtration = filtration
      )
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      invisible(self)
    },

    #' @description This function returns the Betti numbers of the simplicial
    #'   complex.
    #'
    #' @return An integer vector storing the Betti numbers.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$compute_persistence()$betti_numbers()
    #' }
    betti_numbers = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$betti_numbers()
    },

    #' @description Assuming the simplex tree is a 1-skeleton graph, this method
    #'   collapse edges (simplices of higher dimension are ignored) and resets
    #'   the simplex tree from the remaining edges. A good candidate is to build
    #'   a simplex tree on top of a `RipsComplex` of dimension 1 before
    #'   collapsing edges as done in this [Python
    #'   example](https://gudhi.inria.fr/python/latest/_downloads/c82779c19a4ebcf1f96e8e390fe8fdd4/rips_complex_edge_collapse_example.py).
    #'    For implementation details, please refer to
    #'   \insertCite{boissonnat2020edge;textual}{rgudhi}.
    #'
    #' @details It requires `Eigen >= 3.1.0` and an exception is thrown if not
    #'   available.
    #'
    #' ## References
    #'
    #' \insertCited{}
    #'
    #' @param nb_iterations An integer value specifying the number of edge
    #'   collapse iterations to perform. Defaults to `1L`.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$collapse_edges()
    #' }
    collapse_edges = function(nb_iterations = 1) {
      private$m_PythonClass$collapse_edges(nb_iterations = nb_iterations)
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      invisible(self)
    },

    #' @description This function computes the persistence of the simplicial
    #'   complex, so it can be accessed through `$persistent_betti_numbers()`,
    #'   `$persistence_pairs()`, etc. This function is equivalent to
    #'   `$persistence()` when you do not want the list that `$persistence()`
    #'   returns.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    compute_persistence = function(homology_coeff_field = 11,
                                   min_persistence = 0.0,
                                   persistence_dim_max = FALSE) {
      private$m_PythonClass$compute_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence,
        persistence_dim_max = persistence_dim_max
      )
      private$m_ComputedPersistence <- TRUE
      invisible(self)
    },

    #' @description This function returns the dimension of the simplicial
    #'   complex.
    #'
    #' @details This function is not constant time because it can recompute
    #'   dimension if required (can be triggered by `$remove_maximal_simplex()`
    #'   or `$prune_above_filtration()` methods for instance).
    #'
    #' @return An integer value storing the simplicial complex dimension.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$dimension()
    #' }
    dimension = function() {
      private$m_PythonClass$dimension()
    },

    #' @description Expands the simplex tree containing only its one skeleton
    #'   until dimension `max_dim`.
    #'
    #' @details The expanded simplicial complex until dimension `d` attached to
    #'   a graph `G` is the maximal simplicial complex of dimension at most `d`
    #'   admitting the graph `G` as 1-skeleton. The filtration value assigned to
    #'   a simplex is the maximal filtration value of one of its edges.
    #'
    #' The simplex tree must contain no simplex of dimension bigger than 1 when
    #' calling the method.
    #'
    #' @param max_dim An integer value specifying the maximal dimension to
    #'   expented the simplex tree to.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$expansion(2)
    #' }
    expansion = function(max_dim) {
      private$m_PythonClass$expansion(max_dim)
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      invisible(self)
    },

    #' @description Extend filtration for computing extended persistence. This
    #'   function only uses the filtration values at the 0-dimensional
    #'   simplices, and computes the extended persistence diagram induced by the
    #'   lower-star filtration computed with these values.
    #'
    #' @details Note that after calling this function, the filtration values are
    #'   actually modified within the simplex tree. The method
    #'   `$extended_persistence()` retrieves the original values.
    #'
    #' Note that this code creates an extra vertex internally, so you should
    #' make sure that the simplex tree does not contain a vertex with the
    #' largest possible value (i.e., `4294967295`).
    #'
    #' This
    #' [notebook](https://github.com/GUDHI/TDA-tutorial/blob/master/Tuto-GUDHI-extended-persistence.ipynb)
    #' explains how to compute an extension of persistence called extended
    #' persistence.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    extend_filtration = function() {
      private$m_PythonClass$extend_filtration()
      private$m_ComputedExtendedFiltration <- TRUE
      private$m_ComputedPersistence <- FALSE
      invisible(self)
    },

    #' @description This function retrieves good values for extended
    #'   persistence, and separate the diagrams into the Ordinary, Relative,
    #'   Extended+ and Extended- subdiagrams.
    #'
    #' @details The coordinates of the persistence diagram points might be a
    #'   little different than the original filtration values due to the
    #'   internal transformation (scaling to `[-2,-1]`) that is performed on these
    #'   values during the computation of extended persistence.
    #'
    #' This notebook explains how to compute an extension of persistence called
    #' extended persistence.
    #'
    #' @return A list of four persistence diagrams in the format described in
    #'   `$persistence()`. The first one is `Ordinary`, the second one is
    #'   `Relative`, the third one is `Extended+` and the fourth one is
    #'   `Extended-`. See this
    #'   [article](https://link.springer.com/article/10.1007/s10208-008-9027-z)
    #'   and/or Section 2.2 in this
    #'   [article](https://link.springer.com/article/10.1007/s10208-017-9370-z)
    #'   for a description of these subtypes.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$extend_filtration()
    #'   st$extended_persistence()
    #' }
    extended_persistence = function(homology_coeff_field = 11,
                                    min_persistence = 0.0) {
      if (!private$m_ComputedExtendedFiltration)
        cli::cli_abort("You first need to extend the filtration by calling the {.code $extend_filtration()} method.")
      l <- private$m_PythonClass$extended_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence
      )
      names(l) <- c("Ordinary", "Relative", "Extended+", "Extended-")
      l
    },

    #' @description This function returns the filtration value for a given
    #'   N-simplex in this simplicial complex, or +infinity if it is not in the
    #'   complex.
    #'
    #' @return A numeric value storing the filtration value for the input
    #'   N-simplex.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$filtration(0)
    #'   st$filtration(1:2)
    #' }
    filtration = function(simplex) {
      if (length(simplex) == 1)
        simplex <- list(simplex)
      private$m_PythonClass$filtration(simplex)
    },

    #' @description This function returns if the N-simplex was found in the
    #'   simplicial complex or not.
    #'
    #' @return A boolean storing whether the input N-simplex was found in the
    #'   simplicial complex.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$find(0)
    #' }
    find = function(simplex) {
      if (length(simplex) == 1)
        simplex <- list(simplex)
      private$m_PythonClass$find(simplex)
    },

    #' @description Assuming this is a flag complex, this function returns the
    #'   persistence pairs, where each simplex is replaced with the vertices of
    #'   the edges that gave it its filtration value.
    #'
    #' @return A list with the following components:
    #'
    #' - An `n x 3` integer matrix containing the regular persistence pairs of
    #' dimension 0, with one vertex for birth and two for death;
    #' - A list of `m x 4` integer matrices containing the other regular
    #' persistence pairs, grouped by dimension, with 2 vertices per extremity;
    #' - An `l x ?` integer matrix containing the connected components, with one
    #' vertex each;
    #' - A list of `k x 2` integer matrices containing the other essential
    #' features, grouped by dimension, with 2 vertices for birth.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   rc <- RipsComplex$new(data = X, max_edge_length = 1)
    #'   st <- rc$create_simplex_tree(1)
    #'   st$compute_persistence()$flag_persistence_generators()
    #' }
    flag_persistence_generators = function() {
      if (!private$m_IsFlag)
        cli::cli_abort("The current simplex tree is not a flag complex. Please generate a simplex tree from a Rips complex to use this method.")
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$flag_persistence_generators()
    },

    #' @description For a given N-simplex, this function returns a list of
    #'   simplices of dimension N-1 corresponding to the boundaries of the
    #'   N-simplex.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing the (simplicies of the)
    #'   boundary of the input N-simplex in column `simplex` along with their
    #'   corresponding filtration value in column `filtration`.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   splx <- st$get_simplices()$simplex[[1]]
    #'   st$get_boundaries(splx)
    #' }
    get_boundaries = function(simplex) {
      itb <- private$m_PythonClass$get_boundaries(simplex)
      res <- reticulate::iterate(itb)
      res <- purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
      res <- purrr::transpose(res)
      res$filtration <- purrr::flatten_dbl(res$filtration)
      tibble::as_tibble(res)
    },

    #' @description This function returns the cofaces of a given N-simplex with
    #'   a given codimension.
    #'
    #' @param codimension An integer value specifying the codimension. If
    #'   `codimension = 0`, all cofaces are returned (equivalent of
    #'   `$get_star()` function).
    #'
    #' @return A \code{\link[tibble]{tibble}} listing the (simplicies of the)
    #'   cofaces of the input N-simplex in column `simplex` along with their
    #'   corresponding filtration value in column `filtration`.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$get_cofaces(1:2, 0)
    #' }
    get_cofaces = function(simplex, codimension) {
      res <- private$m_PythonClass$get_cofaces(
        simplex = simplex,
        codimension = codimension
      )
      res <- purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
      res <- purrr::transpose(res)
      res$filtration <- purrr::flatten_dbl(res$filtration)
      tibble::as_tibble(res)
    },

    #' @description This function retrieves the list of simplices and their
    #'   given filtration values sorted by increasing filtration values.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing the simplicies in column
    #'   `simplex` along with their corresponding filtration value in column
    #'   `filtration`, in increasing order of filtration value.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$get_filtration()
    #' }
    get_filtration = function() {
      itb <- private$m_PythonClass$get_filtration()
      res <- reticulate::iterate(itb)
      res <- purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
      res <- purrr::transpose(res)
      res$filtration <- purrr::flatten_dbl(res$filtration)
      tibble::as_tibble(res)
    },

    #' @description This function retrieves the list of simplices and their
    #'   given filtration values.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing the simplicies in column
    #'   `simplex` along with their corresponding filtration value in column
    #'   `filtration`, in increasing order of filtration value.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$get_simplices()
    #' }
    get_simplices = function() {
      itb <- private$m_PythonClass$get_simplices()
      res <- reticulate::iterate(itb)
      res <- purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
      res <- purrr::transpose(res)
      res$filtration <- purrr::flatten_dbl(res$filtration)
      tibble::as_tibble(res)
    },

    #' @description This function returns a generator with the (simplices of
    #'   the) skeleton of a maximum given dimension.
    #'
    #' @param dimension A integer value specifying the skeleton dimension value.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing the (simplicies of the)
    #'   skeleton of a maximum dimension in column `simplex` along with their
    #'   corresponding filtration value in column `filtration`.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$get_skeleton(0)
    #' }
    get_skeleton = function(dimension) {
      itb <- private$m_PythonClass$get_skeleton(dimension)
      res <- reticulate::iterate(itb)
      res <- purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
      res <- purrr::transpose(res)
      res$filtration <- purrr::flatten_dbl(res$filtration)
      tibble::as_tibble(res)
    },

    #' @description This function returns the star of a given N-simplex.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing the (simplicies of the)
    #'   star of a simplex in column `simplex` along with their corresponding
    #'   filtration value in column `filtration`.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$get_star(1:2)
    #' }
    get_star = function(simplex) {
      res <- private$m_PythonClass$get_star(simplex)
      res <- purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
      res <- purrr::transpose(res)
      res$filtration <- purrr::flatten_dbl(res$filtration)
      tibble::as_tibble(res)
    },

    #' @description This function inserts the given N-simplex and its subfaces
    #'   with the given filtration value. If some of those simplices are already
    #'   present with a higher filtration value, their filtration value is
    #'   lowered.
    #'
    #' @param filtration A numeric value specifying the filtration value of the
    #'   simplex. Defaults to `0.0`.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly if
    #'   `chainable` is set to `TRUE` (default behavior), or a boolean set to
    #'   `TRUE` if the simplex was not yet in the complex or `FALSE` otherwise
    #'   (whatever its original filtration value).
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$insert(1:2)
    #'   st$insert(1:3, chainable = FALSE)
    #' }
    insert = function(simplex, filtration = 0.0, chainable = TRUE) {
      res <- private$m_PythonClass$insert(
        simplex = simplex,
        filtration = filtration
      )

      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE

      if (chainable) return(invisible(self))
      res
    },

    #' @description Assuming this is a lower-star filtration, this function
    #'   returns the persistence pairs, where each simplex is replaced with the
    #'   vertex that gave it its filtration value.
    #'
    #' @return A list with the following components:
    #'
    #' - A list of `n x 2` integer matrices containing the regular persistence
    #' pairs, grouped by dimension, with one vertex per extremity;
    #' - A list of `m x ?` integer matrices containing the essential features,
    #' grouped by dimension, with one vertex each.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$compute_persistence()$lower_star_persistence_generators()
    #' }
    lower_star_persistence_generators = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$lower_star_persistence_generators()
    },

    #' @description This function ensures that each simplex has a higher
    #'   filtration value than its faces by increasing the filtration values.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly if
    #'   `chainable` is set to `TRUE` (default behavior), or a boolean set to
    #'   `TRUE` if any filtration value was modified or to `FALSE` if the
    #'   filtration was already non-decreasing.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$make_filtration_non_decreasing()
    #' }
    make_filtration_non_decreasing = function(chainable = TRUE) {
      res <- private$m_PythonClass$make_filtration_non_decreasing()

      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE

      if (chainable) return(invisible(self))
      res
    },

    #' @description This function returns the number of simplices of the
    #'   simplicial complex.
    #'
    #' @return An integer value storing the number of simplices in the
    #'   simplicial complex.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$num_simplices()
    #' }
    num_simplices = function() {
      private$m_PythonClass$num_simplices()
    },

    #' @description This function returns the number of vertices of the
    #'   simplicial complex.
    #'
    #' @return An integer value storing the number of vertices in the simplicial
    #'   complex.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$num_vertices()
    #' }
    num_vertices = function() {
      private$m_PythonClass$num_vertices()
    },

    #' @description This function computes and returns the persistence of the
    #'   simplicial complex.
    #'
    #' @return A \code{\link[tibble]{tibble}} listing all persistence feature
    #'   summarised by 3 variables: `dimension`, `birth` and `death`.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$persistence()
    #' }
    persistence = function(homology_coeff_field = 11,
                           min_persistence = 0.0,
                           persistence_dim_max = FALSE) {
      l <- private$m_PythonClass$persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence,
        persistence_dim_max = persistence_dim_max
      )
      l <- purrr::map(l, purrr::simplify_all)
      l <- purrr::map(l, purrr::set_names, nm = c("dimension", "barcode"))

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
    #'   simplicial complex in a specific dimension.
    #'
    #' @param dimension An integer value specifying the desired dimension.
    #'
    #' @return A \code{\link[tibble]{tibble}} storing the persistence intervals
    #'   for the required dimension in two columns `birth` and `death`.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$compute_persistence()$persistence_intervals_in_dimension(1)
    #' }
    persistence_intervals_in_dimension = function(dimension) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      res <- private$m_PythonClass$persistence_intervals_in_dimension(dimension)
      tibble::tibble(birth = res[, 1], death = res[, 2])
    },

    #' @description This function returns a list of persistence birth and death
    #'   simplices pairs.
    #'
    #' @return A list of pairs of integer vectors storing a list of persistence
    #'   simplices intervals.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$compute_persistence()$persistence_pairs()
    #' }
    persistence_pairs = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$persistence_pairs()
    },

    #' @description This function returns the persistent Betti numbers of the
    #'   simplicial complex.
    #'
    #' @param from_value A numeric value specifying the persistence birth limit
    #'   to be added in the numbers (`persistent birth <= from_value`).
    #' @param to_value A numeric value specifying the persistence death limit to
    #'   be added in the numbers (`persistent death > to_value`).
    #'
    #' @return An integer vector storing the persistent Betti numbers.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$compute_persistence()$persistent_betti_numbers(0, 0.1)
    #' }
    persistent_betti_numbers = function(from_value, to_value) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$persistent_betti_numbers(
        from_value = from_value,
        to_value = to_value
      )
    },

    #' @description Prune above filtration value given as parameter.
    #'
    #' @details Note that the dimension of the simplicial complex may be lower
    #'   after calling `prune_above_filtration()` than it was before. However,
    #'   `upper_bound_dimension()` will return the old value, which remains a
    #'   valid upper bound. If you care, you can call `dimension()` method to
    #'   recompute the exact dimension.
    #'
    #' @param filtration A numeric value specifying the maximum threshold value.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly if
    #'   `chainable` is set to `TRUE` (default behavior), or a boolean set to
    #'   `TRUE` if the filtration has been modified or to `FALSE` otherwise.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$prune_above_filtration(0.12)
    #' }
    prune_above_filtration = function(filtration, chainable = TRUE) {
      res <- private$m_PythonClass$prune_above_filtration(filtration)
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      if (chainable) return(invisible(self))
      res
    },

    #' @description This function removes a given maximal N-simplex from the
    #'   simplicial complex.
    #'
    #' @details The dimension of the simplicial complex may be lower after
    #'   calling `$remove_maximal_simplex()` than it was before. However,
    #'   `$upper_bound_dimension()` method will return the old value, which
    #'   remains a valid upper bound. If you care, you can call `$dimension()`
    #'   to recompute the exact dimension.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$remove_maximal_simplex(1:2)
    #' }
    remove_maximal_simplex = function(simplex) {
      private$m_PythonClass$remove_maximal_simplex(simplex)
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      invisible(self)
    },

    #' @description This function resets the filtration value of all the
    #'   simplices of dimension at least `min_dim`. Resets all the simplex tree
    #'   when `min_dim = 0L`. `reset_filtration` may break the filtration
    #'   property with `min_dim > 0`, and it is the user’s responsibility to
    #'   make it a valid filtration (using a large enough `filtration` value, or
    #'   calling `$make_filtration_non_decreasing()` afterwards for instance).
    #'
    #' @param filtration A numeric value specyfing the filtration threshold.
    #' @param min_dim An integer value specifying the minimal dimension.
    #'   Defaults to `0L`.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$reset_filtration(0.1)
    #' }
    reset_filtration = function(filtration, min_dim = 0) {
      private$m_PythonClass$reset_filtration(
        filtration = filtration,
        min_dim = min_dim
      )
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      invisible(self)
    },

    #' @description This function sets the dimension of the simplicial complex.
    #'
    #' @details This function must be used with caution because it disables
    #'   dimension recomputation when required (this recomputation can be
    #'   triggered by `$remove_maximal_simplex()` or
    #'   `$prune_above_filtration()`).
    #'
    #' @param dimension An integer value specifying the dimension.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$set_dimension(1)
    #' }
    set_dimension = function(dimension) {
      private$m_PythonClass$set_dimension(dimension)
      private$m_ComputedPersistence <- FALSE
      private$m_ComputedExtendedFiltration <- FALSE
      invisible(self)
    },

    #' @description This function returns a valid dimension upper bound of the
    #'   simplicial complex.
    #'
    #' @return An integer value storing an upper bound on the dimension of the
    #'   simplicial complex.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   st$upper_bound_dimension()
    #' }
    upper_bound_dimension = function() {
      private$m_PythonClass$upper_bound_dimension()
    },

    #' @description This function writes the persistence intervals of the
    #'   simplicial complex in a user given file name.
    #'
    #' @param persistence_file A string specifying the name of the file.
    #'
    #' @return The updated \code{\link{SimplexTree}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   f <- fs::file_temp(ext = ".dgm")
    #'   st$compute_persistence()$write_persistence_diagram(f)
    #'   fs::file_delete(f)
    #' }
    write_persistence_diagram = function(persistence_file) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$write_persistence_diagram(persistence_file)
      invisible(self)
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedPersistence = FALSE,
    m_ComputedExtendedFiltration = FALSE,
    m_IsFlag = FALSE
  )
)
