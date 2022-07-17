#' R6 Class for Cover Complex
#'
#' @description \loadmathjax Nerves and Graph Induced Complexes are cover
#'   complexes, i.e. simplicial complexes that provably contain topological
#'   information about the input data. They can be computed with a cover of the
#'   data, that comes i.e. from the pre-image of a family of intervals covering
#'   the image of a scalar-valued function defined on the data.
#'
#' @details **Cover complex data structure.** The data structure is a simplicial
#'   complex, representing a Graph Induced simplicial Complex (GIC) or a Nerve,
#'   and whose simplices are computed with a cover \mjseqn{C} of a point cloud
#'   \mjseqn{P}, which often comes from the pre-images of intervals covering the
#'   image of a function \mjseqn{f} defined on \mjseqn{P}. These intervals are
#'   parameterised by their resolution (either their length or their number) and
#'   their gain (percentage of overlap). To compute a GIC, one also needs a
#'   graph \mjseqn{G} built on top of \mjseqn{P}, whose cliques with vertices
#'   belonging to different elements of \mjseqn{C} correspond to the simplices
#'   of the GIC.
#'
#' @author Mathieu Carrière
#' @family filtrations and reconstructions
#'
#' @export
CoverComplex <- R6::R6Class(
  classname = "CoverComplex",
  public = list(
    #' @description `CoverComplex` constructor.
    #'
    #' @return A \code{\link{CoverComplex}} object storing the Cover complex.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc
    #' }
    initialize = function() {
      private$m_PythonClass <- gd$CoverComplex()
    },

    #' @description Computes the extended persistence diagram of the complex.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$compute_PD()
    #' }
    compute_PD = function() {
      private$m_PythonClass$compute_PD()
      private$m_ComputedPersistenceDiagram <- TRUE
      invisible(self)
    },

    #' @description Computes the confidence level of a specific Bottleneck
    #'   distance threshold.
    #'
    #' @param distance_threshold A numeric value specifying the desired
    #'   Bottleneck distance threshold.
    #'
    #' @return A numeric value storing the confidence level corresponding to the
    #'   input threshold on the Bottleneck distance.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$compute_confidence_level_from_distance(distance_threshold = 0.1)
    #' }
    compute_confidence_level_from_distance = function(distance_threshold) {
      private$m_PythonClass$compute_confidence_level_from_distance(distance_threshold)
    },

    #' @description Computes the bottleneck distance threshold corresponding to
    #'   a specific confidence level.
    #'
    #' @param confidence_level A numeric value specifying the desired confidence
    #'   level.
    #'
    #' @return A numeric value storing the threshold on the Bottleneck distance
    #'   corresponding to the input confidence level.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$compute_distance_from_confidence_level(confidence_level = 0.1)
    #' }
    compute_distance_from_confidence_level = function(confidence_level) {
      private$m_PythonClass$compute_distance_from_confidence_level(confidence_level)
    },

    #' @description Computes the distribution of distances via bootstrap.
    #'
    #' @param N An integer value specifying the number of iterations. Defaults to `100L`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$compute_distribution()
    #' }
    compute_distribution = function(N = 100L) {
      private$m_PythonClass$compute_distribution(N)
      private$m_ComputedBootstrapDistribution <- TRUE
      invisible(self)
    },

    #' @description Computes the p-value, i.e. the opposite of the confidence
    #'   level of the largest bottleneck distance preserving the points in the
    #'   persistence diagram of the output simplicial complex.
    #'
    #' @return A numeric value storing the desired p-value.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$compute_p_value()
    #' }
    compute_p_value = function() {
      private$m_PythonClass$compute_p_value()
    },

    #' @return A \code{\link{SimplexTree}} object storing the simplex
    #'   tree created from the Cover complex.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   st <- cc$create_simplex_tree()
    #' }
    create_simplex_tree = function() {
      py_st <- private$m_PythonClass$create_simplex_tree()
      private$m_ComputedSimplexTree <- TRUE
      SimplexTree$new(py_class = py_st)
    },

    #' @description Computes the simplices of the simplicial complex.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$find_simplices()
    #' }
    find_simplices = function() {
      private$m_PythonClass$find_simplices()
      invisible(self)
    },

    #' @description Creates a `.dot` file called `_sc.dot` for neato (part of
    #'   the **graphviz** Python package) once the simplicial complex is
    #'   computed to get a visualization of its 1-skeleton in a `.pdf` file.
    #'
    #' @param dir A character string specifying the path to a directory into
    #'   which the `.dot` file will be saved. Defaults to current working
    #'   directory retrieved via `getwd()`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   withr::with_tempdir({
    #'     cc$plot_dot()
    #'   })
    #' }
    plot_dot = function(dir = getwd()) {
      withr::with_dir(dir, {
        private$m_PythonClass$plot_dot()
      })
      invisible(self)
    }
  ),
  private = list(
    m_PythonClass = NULL,
    m_ComputedPersistenceDiagram = FALSE,
    m_ComputedBootstrapDistribution = FALSE,
    m_ComputedSimplexTree = FALSE
  )
)
