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
#' @param simplex A integer vector representing the N-simplex in the form
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
    #' st <- SimplexTree$new()
    initialize = function(py_class = NULL) {
      if (is.null(py_class))
        private$m_PythonClass <- gd$SimplexTree()
      else
        private$m_PythonClass <- py_class
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
    #' @param filtration A numeric value specifying the new filtration value
    assign_filtration = function(simplex, filtration) {
      private$m_PythonClass$assign_filtration(
        simplex = simplex,
        filtration = filtration
      )
    },

    #' @description This function returns the Betti numbers of the simplicial
    #'   complex.
    #'
    #' @return An integer vector storing the Betti numbers.
    betti_numbers = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$betti_numbers()
    },

    #' @description Assuming the simplex tree is a 1-skeleton graph, this method
    #'   collapse edges (simplices of higher dimension are ignored) and resets
    #'   the simplex tree from the remaining edges. A good candidate is to build
    #'   a simplex tree on top of a RipsComplex of dimension 1 before collapsing
    #'   edges (cf. rips_complex_edge_collapse_example.py). For implementation
    #'   details, please refer to `[6]`.
    #'
    #' @details It requires `Eigen >= 3.1.0` and an exception is thrown if not
    #'   available.
    #'
    #' @param nb_iterations An integer value specifying the number of edge
    #'   collapse iterations to perform. Defaults to `1L`.
    collapse_edges = function(nb_iterations = 1) {
      private$m_PythonClass$collapse_edges(nb_iterations = nb_iterations)
    },

    #' @description This function computes the persistence of the simplicial
    #'   complex, so it can be accessed through `$persistent_betti_numbers()`,
    #'   `$persistence_pairs()`, etc. This function is equivalent to
    #'   `$persistence()` when you do not want the list that `$persistence()`
    #'   returns.
    compute_persistence = function(homology_coeff_field = 11,
                                   min_persistence = 0.0,
                                   persistence_dim_max = FALSE) {
      private$m_PythonClass$compute_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence,
        persistence_dim_max = persistence_dim_max
      )
      private$m_ComputedPersistence <- TRUE
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
    #' X <- replicate(n, runif(2), simplify = FALSE)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' st$dimension()
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
    expansion = function(max_dim) {
      private$m_PythonClass$dimension(max_dim = max_dim)
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
    #' This notebook explains how to compute an extension of persistence called
    #' extended persistence.
    extend_filtration = function() {
      private$m_PythonClass$extend_filtration()
      private$m_ComputedExtendedFiltration <- TRUE
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
    #'   `Extended-`. See
    #'   https://link.springer.com/article/10.1007/s10208-008-9027-z and/or
    #'   section 2.2 in
    #'   https://link.springer.com/article/10.1007/s10208-017-9370-z for a
    #'   description of these subtypes.
    extended_persistence = function(homology_coeff_field = 11,
                                    min_persistence = 0.0) {
      if (!private$m_ComputedExtendedFiltration)
        cli::cli_abort("You first need to extend the filtration by calling the {.code $extend_filtration()} method.")
      private$m_PythonClass$extended_persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence
      )
    },

    #' @description This function returns the filtration value for a given N-simplex in this simplicial complex, or +infinity if it is not in the complex.
    #'
    #' @return A numeric value storing the filtration value for the input N-simplex.
    filtration = function(simplex) {
      private$m_PythonClass$filtration(simplex)
    },

    #' @description This function returns if the N-simplex was found in the
    #'   simplicial complex or not.
    #'
    #' @return A boolean storing whether the input N-simplex was found in the
    #'   simplicial complex.
    find = function(simplex) {
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
    flag_persistence_generators = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$flag_persistence_generators()
    },

    #' @description This function returns a generator with the boundaries of a
    #'   given N-simplex. If you do not need the filtration values, the boundary
    #'   can also be obtained as `itertools.combinations(simplex, len(simplex) -
    #'   1)`.
    #'
    #' @return A list of length-2 lists with components `simplex` (integer
    #'   vector) and `filtration` (numeric value) corresponding to the
    #'   (simplicies of the) boundary of a simplex.
    get_boundaries = function(simplex) {
      itb <- private$m_PythonClass$get_boundaries(simplex)
      res <- reticulate::iterate(itb)
      purrr::map(res, rlang::set_names, nm = c("simplex", "filtration"))
    },

    #' @description This function returns the cofaces of a given N-simplex with
    #'   a given codimension.
    #'
    #' @param codimension An integer value specifying the codimension. If
    #'   `codimension = 0`, all cofaces are returned (equivalent of
    #'   `$get_star()` function).
    #'
    #' @return A list of 2-component lists of the form `(simplex, filtration)`
    #'   storing the (simplices of the) cofaces of a simplex.
    get_cofaces = function(simplex, codimension) {
      private$m_PythonClass$get_cofaces(
        simplex = simplex,
        codimension = codimension
      )
    },

    #' @description This function returns a generator with simplices and their
    #'   given filtration values sorted by increasing filtration values.
    #'
    #' @return A generator with `tuples(simplex, filtration)` pointing to the
    #'   simplices sorted by increasing filtration values.
    get_filtration = function() {
      private$m_PythonClass$get_filtration()
    },

    #' @description This function returns a generator with simplices and their
    #'   given filtration values.
    #'
    #' @return A generator with `tuples(simplex, filtration)` pointing to the
    #'   simplices.
    get_simplices = function() {
      private$m_PythonClass$get_simplices()
    },

    #' @description This function returns a generator with the (simplices of
    #'   the) skeleton of a maximum given dimension.
    #'
    #' @param dimension A integer value specifying the skeleton dimension value.
    #'
    #' @return A generator with `tuples(simplex, filtration)` pointing to the (simplices of the) skeleton of a maximum dimension.
    get_skeleton = function(dimension) {
      private$m_PythonClass$get_skeleton(dimension = dimension)
    },

    #' @description This function returns the star of a given N-simplex.
    #'
    #' @return A list of 2-component lists of the form `(simplex, filtration)`
    #'   storing the (simplices of the) star of a simplex.
    get_star = function(simplex) {
      private$m_PythonClass$get_star(simplex = simplex)
    },

    #' @description This function inserts the given N-simplex and its subfaces
    #'   with the given filtration value. If some of those simplices are already
    #'   present with a higher filtration value, their filtration value is
    #'   lowered.
    #'
    #' @param filtration A numeric value specifying the filtration value of the
    #'   simplex. Defaults to `0.0`.
    #'
    #' @return A boolean set to `TRUE` if the simplex was not yet in the complex
    #'   or `FALSE` otherwise (whatever its original filtration value).
    insert = function(simplex, filtration = 0.0) {
      private$m_PythonClass$insert(
        simplex = simplex,
        filtration = filtration
      )
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
    lower_star_persistence_generators = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$lower_star_persistence_generators()
    },

    #' @description This function ensures that each simplex has a higher
    #'   filtration value than its faces by increasing the filtration values.
    #'
    #' @return A boolean set to `TRUE` if any filtration value was modified or
    #'   to `FALSE` if the filtration was already non-decreasing.
    make_filtration_non_decreasing = function() {
      private$m_PythonClass$make_filtration_non_decreasing()
    },

    #' @description This function returns the number of simplices of the
    #'   simplicial complex.
    #'
    #' @return An integer value storing the number of simplices in the
    #'   simplicial complex.
    num_simplices = function() {
      private$m_PythonClass$num_simplices()
    },

    #' @description This function returns the number of vertices of the
    #'   simplicial complex.
    #'
    #' @return An integer value storing the number of vertices in the simplicial
    #'   complex.
    num_vertices = function() {
      private$m_PythonClass$num_vertices()
    },

    #' @description This function computes and returns the persistence of the
    #'   simplicial complex.
    #'
    #' @return A list of `pairs(dimension, pair(birth, death))` storing the
    #'   persistence of the simplicial complex.
    persistence = function(homology_coeff_field = 11,
                           min_persistence = 0.0,
                           persistence_dim_max = FALSE) {
      private$m_PythonClass$persistence(
        homology_coeff_field = homology_coeff_field,
        min_persistence = min_persistence,
        persistence_dim_max = persistence_dim_max
      )
    },

    #' @description This function returns the persistence intervals of the
    #'   simplicial complex in a specific dimension.
    #'
    #' @param dimension An integer value specifying the desired dimension.
    #'
    #' @return A `? x 2` numeric matrix storing the persistence intervals.
    persistence_intervals_in_dimension = function(dimension) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$persistence_intervals_in_dimension(
        dimension = dimension
      )
    },

    #' @description This function returns a list of persistence birth and death
    #'   simplices pairs.
    #'
    #' @return A list of pairs of integer vectors storing a list of persistence
    #'   simplices intervals.
    persistence_pairs = function() {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$persistence_pairs
    },

    #' @description This function returns the persistent Betti numbers of the
    #'   simplicial complex.
    #'
    #' @param from_value A numeric value specifying the persistence birth limit to be added in the numbers (`persistent birth <= from_value`).
    #' @param to_value A numeric value specifying the persistence death limit to be added in the numbers (`persistent death > to_value`).
    #'
    #' @return An integer vector storing the persistent Betti numbers.
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
    #' @return A boolean set to `TRUE` if the filtration has been modified or to
    #'   `FALSE` otherwise.
    prune_above_filtration = function(filtration) {
      private$m_PythonClass$prune_above_filtration(filtration = filtration)
    },

    #' @description This function removes a given maximal N-simplex from the
    #'   simplicial complex.
    #'
    #' @details The dimension of the simplicial complex may be lower after
    #'   calling `$remove_maximal_simplex()` than it was before. However,
    #'   `$upper_bound_dimension()` method will return the old value, which
    #'   remains a valid upper bound. If you care, you can call `$dimension()`
    #'   to recompute the exact dimension.
    remove_maximal_simplex = function(simplex) {
      private$m_PythonClass$remove_maximal_simplex(simplex = simplex)
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
    reset_filtration = function(filtration, min_dim = 0) {
      private$m_PythonClass$reset_filtration(
        filtration = filtration,
        min_dim = min_dim
      )
    },

    #' @description This function sets the dimension of the simplicial complex.
    #'
    #' @details This function must be used with caution because it disables
    #'   dimension recomputation when required (this recomputation can be
    #'   triggered by `$remove_maximal_simplex()` or
    #'   `$prune_above_filtration()`).
    #'
    #' @param dimension An integer value specifying the dimension.
    set_dimension = function(dimension) {
      private$m_PythonClass$set_dimension(dimension = dimension)
    },

    #' @description This function returns a valid dimension upper bound of the
    #'   simplicial complex.
    #'
    #' @return An integer value storing an upper bound on the dimension of the
    #'   simplicial complex.
    upper_bound_dimension = function() {
      private$m_PythonClass$upper_bound_dimension()
    },

    #' @description This function writes the persistence intervals of the
    #'   simplicial complex in a user given file name.
    #'
    #' @param persistence_file A string specifying the name of the file.
    write_persistence_diagram = function(persistence_file) {
      if (!private$m_ComputedPersistence)
        cli::cli_abort("You first need to compute the persistence by calling the {.code $compute_persistence()} method.")
      private$m_PythonClass$write_persistence_diagram(
        persistence_file = persistence_file
      )
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedPersistence = FALSE,
    m_ComputedExtendedFiltration = FALSE
  )
)
