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
    #' @param N An integer value specifying the number of iterations. Defaults
    #'   to `100L`.
    #' @param dir A character string specifying the path to a directory into
    #'   which the `.off` file will be saved. Defaults to current working
    #'   directory retrieved via `getwd()`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$compute_distribution()
    #' }
    compute_distribution = function(N = 100L, dir = getwd()) {
      withr::with_dir(dir, {
        private$m_PythonClass$compute_distribution(N)
      })
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
    },

    #' @description Creates a `.off` file called `_sc.off` for 3D visualization,
    #'   which contains the 2-skeleton of the GIC. This function assumes that
    #'   the cover has been computed with Voronoi. If data points are in 1D or
    #'   2D, the remaining coordinates of the points embedded in 3D are set to
    #'   0.
    #'
    #' @param dir A character string specifying the path to a directory into
    #'   which the `.off` file will be saved. Defaults to current working
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
    plot_off = function(dir = getwd()) {
      withr::with_dir(dir, {
        private$m_PythonClass$plot_off()
      })
      invisible(self)
    },

    #' @description Reads and stores the input point cloud from a `.(n)OFF`
    #'   file.
    #'
    #' @param off_file A character string specifying the location of the
    #'   `.(n)OFF` file to read the point cloud from.
    #' @param chainable A boolean specyfing whether the method should be
    #'   chainable in which case it returns invisibly the class itself. Defaults
    #'   to `TRUE`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly if
    #'   `chainable = TRUE` or a boolean storing the read file status.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #'   cc$read_point_cloud(url)
    #' }
    read_point_cloud = function(off_file, chainable = TRUE) {
      if (substr(off_file, 1, 5) == "https") {
        withr::with_tempfile("tf", {
          download_file(off_file, tf)
          res <- private$m_PythonClass$read_point_cloud(tf)
        })
      } else
        res <- private$m_PythonClass$read_point_cloud(off_file)
      if (chainable) return(invisible(self))
      res
    },

    #' @description Computes the optimal length of intervals (i.e. the smallest
    #'   interval length avoiding discretization artifacts - see
    #'   \insertCite{carriere2018statistical;textual}{rgudhi}) for a functional
    #'   cover.
    #'
    #' ## References
    #' \insertAllCited{}
    #'
    #' @param chainable A boolean specyfing whether the method should be
    #'   chainable in which case it returns invisibly the class itself. Defaults
    #'   to `TRUE`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly if
    #'   `chainable = TRUE` or a numeric value storing the resolution interval
    #'   length used to compute the cover.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_automatic_resolution()
    #' }
    set_automatic_resolution = function(chainable = TRUE) {
      res <- private$m_PythonClass$set_automatic_resolution()
      if (chainable) return(invisible(self))
      res
    },

    #' @description Computes the function used to color the nodes of the
    #'   simplicial complex from the k-th coordinate.
    #'
    #' @param k An integer value specifying the coordinate to use (start at 0).
    #'   Defaults to `0L`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_color_from_coordinate()
    #' }
    set_color_from_coordinate = function(k = 0) {
      private$m_PythonClass$set_color_from_coordinate(k = k)
      invisible(self)
    },

    #' @description Computes the function used to color the nodes of the
    #'   simplicial complex from a file containing the function values.
    #'
    #' @param color_file_name A character string specifying the name of the
    #'   input color file.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_color_from_file()
    #' }
    set_color_from_file = function(color_file_name) {
      private$m_PythonClass$set_color_from_file(color_file_name = color_file_name)
      invisible(self)
    },

    #' @description Computes the function used to color the nodes of the
    #'   simplicial complex from a vector stored in memory.
    #'
    #' @param color_values A numeric vector specifying the input vector of
    #'   values.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_color_from_range(seq(0, 1, len = 100))
    #' }
    set_color_from_range = function(color_values) {
      private$m_PythonClass$set_color_from_range(color_values)
      invisible(self)
    },

    #' @description Creates the cover \mjseqn{C} from the Voronoï cells of a
    #'   subsampling of the point cloud.
    #'
    #' @param m An integer value specifying the number of points in the
    #'   subsample. Defaults to `100L`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_cover_from_Voronoi()
    #' }
    set_cover_from_Voronoi = function(m = 100L) {
      private$m_PythonClass$set_cover_from_Voronoi(m = m)
      invisible(self)
    },

    #' @description Creates the cover \mjseqn{C} from a file containing the
    #'   cover elements of each point (the order has to be the same as in the
    #'   input file!).
    #'
    #' @param cover_file_name A character string specifying the path to the
    #'   input cover file.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_cover_from_file()
    #' }
    set_cover_from_file = function(cover_file_name) {
      private$m_PythonClass$set_cover_from_file(cover_file_name = cover_file_name)
      invisible(self)
    },

    #' @description Creates a cover \mjseqn{C} from the pre-images of the
    #'   function \mjseqn{f}.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_cover_from_function()
    #' }
    set_cover_from_function = function() {
      private$m_PythonClass$set_cover_from_function()
      invisible(self)
    },

    #' @description Reads and stores the input distance matrix from a vector
    #'   stored in memory.
    #'
    #' @param distance_matrix A numeric matrix specifying the distance matrix.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' D <- dist(iris[, -5])
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_distances_from_range(D)
    #' }
    set_distances_from_range = function(distance_matrix) {
      if (inherits(distance_matrix, "dist"))
        distance_matrix <- as.matrix(distance_matrix)
      if (!is.matrix(distance_matrix))
        cli::cli_abort("The input should be either a matrix or an object of class {.code dist}.")
      private$m_PythonClass$set_distances_from_range(distance_matrix)
      invisible(self)
    },

    #' @description Creates the function \mjseqn{f} from the \mjseqn{k}-*th*
    #'   coordinate of the point cloud.
    #'
    #' @param k An integer value specifying the coordinate to use (starts at
    #'   `0L`). Defaults to `0L`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_function_from_coordinate()
    #' }
    set_function_from_coordinate = function(k = 0L) {
      private$m_PythonClass$set_function_from_coordinate(k)
      invisible(self)
    },

    #' @description Creates the function \mjseqn{f} from a file containing the
    #'   function values.
    #'
    #' @param func_file_name A character string specifying the path to the input
    #'   function file.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_function_from_file()
    #' }
    set_function_from_file = function(func_file_name) {
      private$m_PythonClass$set_function_from_file(func_file_name = func_file_name)
      invisible(self)
    },

    #' @description Creates the function \mjseqn{f} from a vector stored in
    #'   memory.
    #'
    #' @param function_values A numeric vector specifying the function values to
    #'   be used.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_function_from_range(seq(0, 1, len = 100))
    #' }
    set_function_from_range = function(function_values) {
      private$m_PythonClass$set_function_from_range(function_values)
      invisible(self)
    },

    #' @description Sets a gain from a value stored in memory.
    #'
    #' @param g A numeric value specifying the gain. Defaults to `0.3`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_gain()
    #' }
    set_gain = function(g = 0.3) {
      private$m_PythonClass$set_gain(g = g)
      invisible(self)
    },

    #' @description Creates a graph \mjseqn{G} from the triangulation given by
    #'   the input `.off` file.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_graph_from_OFF()
    #' }
    set_graph_from_OFF = function() {
      private$m_PythonClass$set_graph_from_OFF()
      invisible(self)
    },

    #' @description Creates a graph \mjseqn{G} from a Rips complex whose
    #'   threshold value is automatically tuned with subsampling - see
    #'   \insertCite{carriere2018statistical;textual}{rgudhi}.
    #'
    #' ## References
    #' \insertAllCited{}
    #'
    #' @param N An integer value specifying the number of subsampling
    #'   iterations. Defaults to `100L` but there is no guarantee on how to
    #'   choose it.
    #' @param chainable A boolean specyfing whether the method should be
    #'   chainable in which case it returns invisibly the class itself. Defaults
    #'   to `TRUE`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly if
    #'   `chainable = TRUE` or a numeric value storing the delta threshold used
    #'   for computing the Rips complex..
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_graph_from_automatic_rips(chainable = FALSE)
    #' }
    set_graph_from_automatic_rips = function(N = 100L, chainable = TRUE) {
      res <- private$m_PythonClass$set_graph_from_automatic_rips()
      if (chainable) return(invisible(self))
      res
    },

    #' @description Creates a graph \mjseqn{G} from a file containing the edges.
    #'
    #' @param graph_file_name A character string specifying the path to the
    #'   input graph file. The graph file contains one edge per line, each edge
    #'   being represented by the IDs of its two nodes.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   # cc$set_graph_from_file()
    #' }
    set_graph_from_file = function(graph_file_name) {
      private$m_PythonClass$set_graph_from_file(graph_file_name = graph_file_name)
      invisible(self)
    },

    #' @description Creates a graph \mjseqn{G} from a Rips complex.
    #'
    #' @param threshold A numeric value specifying the threshold value for the
    #'   Rips complex.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_graph_from_rips(0.1)
    #' }
    set_graph_from_rips = function(threshold) {
      private$m_PythonClass$set_graph_from_rips(threshold)
      invisible(self)
    },

    #' @description Sets the mask, which is a threshold integer such that nodes
    #'   in the complex that contain a number of data points which is less than
    #'   or equal to this threshold are not displayed.
    #'
    #' @param nodemask A numeric value specifying the threshold value for
    #'   generating the mask.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_mask(0.5)
    #' }
    set_mask = function(nodemask) {
      private$m_PythonClass$set_mask(nodemask)
      invisible(self)
    },

    #' @description Reads and stores the input point cloud from a vector stored
    #'   in memory.
    #'
    #' @param cloud A numeric matrix specifying the coordinates of the point
    #'   cloud.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n),
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' X <- Reduce(rbind, X, init = numeric())
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_point_cloud_from_range(X)
    #' }
    set_point_cloud_from_range = function(cloud) {
      private$m_PythonClass$set_point_cloud_from_range(cloud)
      invisible(self)
    },

    #' @description Sets a length of intervals from a value stored in memory.
    #'
    #' @param resolution A numeric value specifying the length of intervals.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_resolution_with_interval_length(1)
    #' }
    set_resolution_with_interval_length = function(resolution) {
      private$m_PythonClass$set_resolution_with_interval_length(resolution)
      invisible(self)
    },

    #' @description Sets a number of intervals from a value stored in memory.
    #'
    #' @param resolution An integer value specifying the number of intervals.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_resolution_with_interval_number(100L)
    #' }
    set_resolution_with_interval_number = function(resolution) {
      private$m_PythonClass$set_resolution_with_interval_number(resolution)
      invisible(self)
    },

    #' @description Sets the constants used to subsample the data set. These
    #'   constants are explained in
    #'   \insertCite{carriere2018statistical;textual}{rgudhi}.
    #'
    #' ## References
    #' \insertAllCited{}
    #'
    #' @param constant A numeric value specifying the subsampling constant.
    #' @param power A numeric value specifying the subsampling power.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_subsampling(constant = 0, power = 1)
    #' }
    set_subsampling = function(constant, power) {
      private$m_PythonClass$set_subsampling(
        constant = constant,
        power = power
      )
      invisible(self)
    },

    #' @description Specifies the type of the output simplicial complex.
    #'
    #' @param type A character string specifying the type of output simplicial
    #'   complex. Can be either `"GIC"` or `"Nerve"`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_type("GIC")
    #' }
    set_type = function(type) {
      private$m_PythonClass$set_type(type)
      invisible(self)
    },


    #' @description Specifies whether the program should display information or
    #'   not.
    #'
    #' @param verbose A boolean specifying whether to display information.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$set_verbose(FALSE)
    #' }
    set_verbose = function(verbose) {
      private$m_PythonClass$set_verbose(verbose)
      invisible(self)
    },

    #' @description Returns the data subset corresponding to a specific node of
    #'   the created complex.
    #'
    #' @param node_id An integer value specifying the ID of the desired node.
    #'
    #' @return An integer vector storing the IDs of the data points at the input
    #'   node.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$subpopulation(0)
    #' }
    subpopulation = function(node_id) {
      private$m_PythonClass$subpopulation(node_id)
    },

    #' @description Creates a `.txt` file called `_sc.txt` describing the
    #'   1-skeleton, which can then be plotted with *e.g.* KeplerMapper.
    #'
    #' @param dir A character string specifying the path to a directory into
    #'   which the `.txt` file will be saved. Defaults to current working
    #'   directory retrieved via `getwd()`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examples
    #' if (reticulate::py_module_available("gudhi")) {
    #'   cc <- CoverComplex$new()
    #'   cc$write_info()
    #' }
    write_info = function(dir = getwd()) {
      withr::with_dir(dir, {
        private$m_PythonClass$write_info()
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
