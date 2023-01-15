#' R6 Class for Cover Complex
#'
#' @description Nerves and Graph Induced Complexes are cover complexes, i.e.
#'   simplicial complexes that provably contain topological information about
#'   the input data. They can be computed with a cover of the data, that comes
#'   i.e. from the pre-image of a family of intervals covering the image of a
#'   scalar-valued function defined on the data.
#'
#' @details **Cover complex data structure.** The data structure is a simplicial
#'   complex, representing a Graph Induced simplicial Complex (GIC) or a Nerve,
#'   and whose simplices are computed with a cover \eqn{C} of a point cloud
#'   \eqn{P}, which often comes from the pre-images of intervals covering the
#'   image of a function \eqn{f} defined on \eqn{P}. These intervals are
#'   parameterised by their resolution (either their length or their number) and
#'   their gain (percentage of overlap). To compute a GIC, one also needs a
#'   graph \eqn{G} built on top of \eqn{P}, whose cliques with vertices
#'   belonging to different elements of \eqn{C} correspond to the simplices of
#'   the GIC.
#'
#' @author Mathieu Carrière
#' @family filtrations and reconstructions
#'
#' @export
CoverComplex <- R6::R6Class(
  classname = "CoverComplex",
  inherit = PythonClass,
  public = list(
    #' @description `CoverComplex` constructor.
    #'
    #' @param type A character string specifying the type of output simplicial
    #'   complex. Can be either `"GIC"` or `"Nerve"`.
    #'
    #' @return A \code{\link{CoverComplex}} object storing the Cover complex.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc
    initialize = function(type) {
      type <- match.arg(type, choices = c("GIC", "Nerve"))
      super$set_python_class(
        gd$CoverComplex()
      )
      super$get_python_class()$set_type(type)
    },

    #' @description Computes the extended persistence diagram of the complex.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   compute_PD()
    compute_PD = function() {
      if (!private$m_ComputedSimplicies)
        cli::cli_abort("You first need to run the {.code $find_simplicies()} method.")
      super$get_python_class()$compute_PD()
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   compute_distribution()$
    #'   compute_confidence_level_from_distance(distance_threshold = 0.1)
    compute_confidence_level_from_distance = function(distance_threshold) {
      if (!private$m_ComputedBootstrapDistribution)
        cli::cli_abort("You first need to compute the bootstrap distribution using the {.code $compute_distribution()} method.")
      super$get_python_class()$compute_confidence_level_from_distance(distance_threshold)
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   compute_distribution()$
    #'   compute_distance_from_confidence_level(confidence_level = 0.95)
    compute_distance_from_confidence_level = function(confidence_level) {
      if (!private$m_ComputedBootstrapDistribution)
        cli::cli_abort("You first need to compute the bootstrap distribution using the {.code $compute_distribution()} method.")
      super$get_python_class()$compute_distance_from_confidence_level(confidence_level)
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   compute_distribution()
    compute_distribution = function(N = 100L, dir = getwd()) {
      if (!private$m_ComputedSimplicies)
        cli::cli_abort("You first need to run the {.code $find_simplicies()} method.")
      withr::with_dir(dir, {
        super$get_python_class()$compute_distribution(N)
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   compute_distribution()$
    #'   compute_p_value()
    compute_p_value = function() {
      if (!private$m_ComputedBootstrapDistribution)
        cli::cli_abort("You first need to compute the bootstrap distribution using the {.code $compute_distribution()} method.")
      super$get_python_class()$compute_p_value()
    },

    #' @return A \code{\link{SimplexTree}} object storing the simplex
    #'   tree created from the Cover complex.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' st <- cc$
    #'   read_point_cloud(url)$
    #'   create_simplex_tree()
    create_simplex_tree = function() {
      py_st <- super$get_python_class()$create_simplex_tree()
      private$m_ComputedSimplexTree <- TRUE
      SimplexTree$new(py_class = py_st)
    },

    #' @description Computes the simplices of the simplicial complex.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()
    find_simplices = function() {
      if (!private$ m_IsCoverDefined)
        cli::cli_abort("You first need to register a cover to the class using one of the {.code $set_cover_*()} methods before calling the {.code $find_simplicies()} method.")
      if (private$m_IsCoverDefinedFromFunction && !private$m_IsResolutionDefined)
        cli::cli_abort("The cover has been registered from a function but the resolution seems not to have been set. Please call one of the {.code $set_resolution_*()} methods or the {.code $set_automatic_resolution()} method first.")
      super$get_python_class()$find_simplices()
      private$m_ComputedSimplicies <- TRUE
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()
    #' withr::with_tempdir({
    #'   cc$plot_dot()
    #' })
    plot_dot = function(dir = getwd()) {
      withr::with_dir(dir, {
        super$get_python_class()$plot_dot()
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()
    #' withr::with_tempdir({
    #'   cc$plot_dot()
    #' })
    plot_off = function(dir = getwd()) {
      withr::with_dir(dir, {
        super$get_python_class()$plot_off()
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)
    read_point_cloud = function(off_file, chainable = TRUE) {
      if (substr(off_file, 1, 5) == "https") {
        withr::with_tempfile("tf", {
          download_file(off_file, tf)
          res <- super$get_python_class()$read_point_cloud(tf)
        })
      } else
        res <- super$get_python_class()$read_point_cloud(off_file)
      private$m_IsPointCloudDefined <- TRUE
      private$m_IsPointCloudDefinedFromOFF <- TRUE
      if (chainable) return(invisible(self))
      res
    },

    #' @description Computes the optimal length of intervals (i.e. the smallest
    #'   interval length avoiding discretization artifacts - see
    #'   \insertCite{carriere2018statistical;textual}{rgudhi}) for a functional
    #'   cover.
    #'
    #' ## References
    #' \insertCited{}
    #'
    #' @param chainable A boolean specyfing whether the method should be
    #'   chainable in which case it returns invisibly the class itself. Defaults
    #'   to `TRUE`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly if
    #'   `chainable = TRUE` or a numeric value storing the resolution interval
    #'   length used to compute the cover.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()
    set_automatic_resolution = function(chainable = TRUE) {
      if (!private$m_IsCoverDefined)
        cli::cli_abort("You first need to register a cover to the class using one of the {.code $set_cover_*()} methods.")
      if (!private$m_IsCoverDefinedFromFunction)
        cli::cli_abort("The cover must have been registered via the {.code $set_cover_from_function()} method.")
      res <- super$get_python_class()$set_automatic_resolution()
      private$m_IsResolutionDefined <- TRUE
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()
    set_color_from_coordinate = function(k = 0) {
      if (!private$m_IsPointCloudDefined)
        cli::cli_abort("Please first register a point cloud via either the {.code $read_point_cloud()} method or the {.code $set_point_cloud_from_range()} method.")
      super$get_python_class()$set_color_from_coordinate(k = k)
      invisible(self)
    },

    #' @description Computes the function used to color the nodes of the
    #'   simplicial complex from a file containing the function values.
    #'
    #' @details The color file should be a `.txt` file with as many lines as
    #'   there are points in the point cloud. Each line should be populated with
    #'   a single numeric value.
    #'
    #' @param color_file_name A character string specifying the name of the
    #'   input color file.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cf <- system.file("extdata", "color_file.txt", package = "rgudhi")
    #' cc <- CoverComplex$
    #'   new(type = "GIC")$
    #'   read_point_cloud(url)$
    #'   set_color_from_file(cf)
    set_color_from_file = function(color_file_name) {
      super$get_python_class()$set_color_from_file(color_file_name)
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_range(seq(0, 1, len = 100))
    set_color_from_range = function(color_values) {
      super$get_python_class()$set_color_from_range(color_values)
      invisible(self)
    },

    #' @description Creates the cover \eqn{C} from the Voronoï cells of a
    #'   subsampling of the point cloud.
    #'
    #' @param m An integer value specifying the number of points in the
    #'   subsample. Defaults to `100L`.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_Voronoi()
    set_cover_from_Voronoi = function(m = 100L) {
      if (!private$m_IsGraphDefined)
        cli::cli_abort("You first need to register a graph to the class using one of the {.code $set_graph_*()} methods before calling any of the {.code $set_cover_*()} methods.")
      super$get_python_class()$set_cover_from_Voronoi(m = m)
      private$m_IsCoverDefined <- TRUE
      private$m_IsCoverDefinedFromFunction <- FALSE
      invisible(self)
    },

    #' @description Creates the cover \eqn{C} from a file containing the cover
    #'   elements of each point (the order has to be the same as in the input
    #'   file!).
    #'
    #' @details The cover file should be a `.txt` file with as many lines as
    #'   there are points in the point cloud. Each line should be populated with
    #'   an integer vector specifying to which cover elements does each point
    #'   belong.
    #'
    #' @param cover_file_name A character string specifying the path to the
    #'   input cover file.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cf <- system.file("extdata", "cover_file.txt", package = "rgudhi")
    #' cc <- CoverComplex$
    #'   new(type = "GIC")$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()#$
    #'   #set_cover_from_file(cf) # TODO: fix in python
    set_cover_from_file = function(cover_file_name) {
      if (!private$m_IsGraphDefined)
        cli::cli_abort("You first need to register a graph to the class using one of the {.code $set_graph_*()} methods before calling any of the {.code $set_cover_*()} methods.")
      super$get_python_class()$set_cover_from_file(cover_file_name)
      private$m_IsCoverDefined <- TRUE
      private$m_IsCoverDefinedFromFunction <- FALSE
      invisible(self)
    },

    #' @description Creates a cover \eqn{C} from the pre-images of the function
    #'   \eqn{f}.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()
    set_cover_from_function = function() {
      if (!private$m_IsGraphDefined)
        cli::cli_abort("You first need to register a graph to the class using one of the {.code $set_graph_*()} methods before calling any of the {.code $set_cover_*()} methods.")
      if (!private$m_IsFunctionDefined)
        cli::cli_abort("You first need to register a function to the class using one of the {.code $set_function_*()} methods before calling the {.code $set_cover_from_function()} method.")
      super$get_python_class()$set_cover_from_function()
      private$m_IsCoverDefined <- TRUE
      private$m_IsCoverDefinedFromFunction <- TRUE
      invisible(self)
    },

    #' @description Reads and stores the input distance matrix from a vector
    #'   stored in memory.
    #'
    #' @param distance_matrix A numeric matrix specifying the distance matrix.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' D <- dist(iris[, -5])
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$set_distances_from_range(D)
    set_distances_from_range = function(distance_matrix) {
      if (inherits(distance_matrix, "dist"))
        distance_matrix <- as.matrix(distance_matrix)
      if (!is.matrix(distance_matrix))
        cli::cli_abort("The input should be either a matrix or an object of class {.code dist}.")
      super$get_python_class()$set_distances_from_range(distance_matrix)
      invisible(self)
    },

    #' @description Creates the function \eqn{f} from the \eqn{k}-*th*
    #'   coordinate of the point cloud.
    #'
    #' @param k An integer value specifying the coordinate to use (starts at
    #'   `0L`). Defaults to `0L`.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()
    set_function_from_coordinate = function(k = 0L) {
      if (!private$m_IsPointCloudDefined)
        cli::cli_abort("Please first register a point cloud via either the {.code $read_point_cloud()} method or the {.code $set_point_cloud_from_range()} method.")
      super$get_python_class()$set_function_from_coordinate(k)
      private$m_IsFunctionDefined <- TRUE
      invisible(self)
    },

    #' @description Creates the function \eqn{f} from a file containing the
    #'   function values.
    #'
    #' @details The function file should be a `.txt` file with as many lines as
    #'   there are points in the point cloud. Each line should be populated with
    #'   a single numeric value.
    #'
    #' @param func_file_name A character string specifying the path to the input
    #'   function file.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' ff <- system.file("extdata", "function_file.txt", package = "rgudhi")
    #' cc <- CoverComplex$
    #'   new(type = "GIC")$
    #'   read_point_cloud(url)$
    #'   set_function_from_file(ff)
    set_function_from_file = function(func_file_name) {
      super$get_python_class()$set_function_from_file(func_file_name)
      private$m_IsFunctionDefined <- TRUE
      invisible(self)
    },

    #' @description Creates the function \eqn{f} from a vector stored in memory.
    #'
    #' @param function_values A numeric vector specifying the function values to
    #'   be used.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_range(seq(0, 1, len = 100))
    set_function_from_range = function(function_values) {
      super$get_python_class()$set_function_from_range(function_values)
      private$m_IsFunctionDefined <- TRUE
      invisible(self)
    },

    #' @description Sets a gain from a value stored in memory.
    #'
    #' @param g A numeric value specifying the gain. Defaults to `0.3`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$set_gain()
    set_gain = function(g = 0.3) {
      super$get_python_class()$set_gain(g = g)
      invisible(self)
    },

    #' @description Creates a graph \eqn{G} from the triangulation given by the
    #'   input `.off` file.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_OFF()
    set_graph_from_OFF = function() {
      if (!private$m_IsPointCloudDefined)
        cli::cli_abort("You first need to register a point cloud to the class using either the {.code $read_point_cloud()} method or the {.code $set_point_cloud_from_range()} mehtod before calling any of the {.code $set_graph_*()} methods.")
      if (!private$m_IsPointCloudDefinedFromOFF)
        cli::cli_abort("This method attempts to re-use the OFF file given for registering the point cloud but the point cloud has not been set via an OFF file. Please call the {.code $read_point_cloud()} method first.")
      super$get_python_class()$set_graph_from_OFF()
      private$m_IsGraphDefined <- TRUE
      invisible(self)
    },

    #' @description Creates a graph \eqn{G} from a Rips complex whose threshold
    #'   value is automatically tuned with subsampling - see
    #'   \insertCite{carriere2018statistical;textual}{rgudhi}.
    #'
    #' ## References
    #' \insertCited{}
    #'
    #' @param N An integer value specifying the number of subsampling
    #'   iterations. Defaults to `100L` but there is no guarantee on how to
    #'   choose it.
    #' @param chainable A boolean specyfing whether the method should be
    #'   chainable in which case it returns invisibly the class itself. Defaults
    #'   to `TRUE`.
    #'
    #' @return The updated [CoverComplex] class itself invisibly if `chainable =
    #'   TRUE` or a numeric value storing the delta threshold used for computing
    #'   the Rips complex..
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()
    set_graph_from_automatic_rips = function(N = 100L, chainable = TRUE) {
      if (!private$m_IsPointCloudDefined)
        cli::cli_abort("You first need to register a point cloud to the class using either the {.code $read_point_cloud()} method or the {.code $set_point_cloud_from_range()} mehtod before calling any of the {.code $set_graph_*()} methods.")
      if (private$m_IsGraphDefinedFromAutomaticRips)
        return(invisible(self))
      res <- super$get_python_class()$set_graph_from_automatic_rips(N)
      private$m_IsGraphDefined <- TRUE
      private$m_IsGraphDefinedFromAutomaticRips <- TRUE
      if (chainable) return(invisible(self))
      res
    },

    #' @description Creates a graph \eqn{G} from a file containing the edges.
    #'
    #' @details The file should contain the edge list of the graph by rows, each
    #'   row reporting the indices of the two connected vertices as stored in
    #'   the input point cloud.
    #'
    #' @param graph_file_name A character string specifying the path to the
    #'   input graph file. The graph file contains one edge per line, each edge
    #'   being represented by the IDs of its two nodes.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' gf <- system.file("extdata", "graph_file.txt", package = "rgudhi")
    #' cc <- CoverComplex$
    #'   new(type = "GIC")$
    #'   read_point_cloud(url)$
    #'   set_graph_from_file(gf)
    set_graph_from_file = function(graph_file_name) {
      if (!private$m_IsPointCloudDefined)
        cli::cli_abort("You first need to register a point cloud to the class using either the {.code $read_point_cloud()} method or the {.code $set_point_cloud_from_range()} mehtod before calling any of the {.code $set_graph_*()} methods.")
      super$get_python_class()$set_graph_from_file(graph_file_name)
      private$m_IsGraphDefined <- TRUE
      invisible(self)
    },

    #' @description Creates a graph \eqn{G} from a Rips complex.
    #'
    #' @param threshold A numeric value specifying the threshold value for the
    #'   Rips complex.
    #'
    #' @return The updated [CoverComplex] class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_rips(0.1)
    set_graph_from_rips = function(threshold) {
      if (!private$m_IsPointCloudDefined)
        cli::cli_abort("You first need to register a point cloud to the class using either the {.code $read_point_cloud()} method or the {.code $set_point_cloud_from_range()} mehtod before calling any of the {.code $set_graph_*()} methods.")
      super$get_python_class()$set_graph_from_rips(threshold)
      private$m_IsGraphDefined <- TRUE
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   set_mask(1)
    set_mask = function(nodemask) {
      if (!private$m_ComputedSimplicies)
        cli::cli_abort("You need to first run the {.code $find_simplicies()} method before calling the {.code $set_mask()} method.")
      super$get_python_class()$set_mask(nodemask)
      private$m_ComputedBootstrapDistribution <- FALSE
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' X <- Reduce(rbind, X, init = numeric())
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   set_point_cloud_from_range(X)
    set_point_cloud_from_range = function(cloud) {
      super$get_python_class()$set_point_cloud_from_range(cloud)
      private$m_IsPointCloudDefined <- TRUE
      private$m_IsPointCloudDefinedFromOFF <- FALSE
      invisible(self)
    },

    #' @description Sets a length of intervals from a value stored in memory.
    #'
    #' @param resolution A numeric value specifying the length of intervals.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_length(1)
    set_resolution_with_interval_length = function(resolution) {
      super$get_python_class()$set_resolution_with_interval_length(resolution)
      private$m_IsResolutionDefined <- TRUE
      invisible(self)
    },

    #' @description Sets a number of intervals from a value stored in memory.
    #'
    #' @param resolution An integer value specifying the number of intervals.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)
    set_resolution_with_interval_number = function(resolution) {
      super$get_python_class()$set_resolution_with_interval_number(resolution)
      private$m_IsResolutionDefined <- TRUE
      invisible(self)
    },

    #' @description Sets the constants used to subsample the data set. These
    #'   constants are explained in
    #'   \insertCite{carriere2018statistical;textual}{rgudhi}.
    #'
    #' ## References
    #' \insertCited{}
    #'
    #' @param constant A numeric value specifying the subsampling constant.
    #' @param power A numeric value specifying the subsampling power.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$set_subsampling(constant = 0, power = 1)
    set_subsampling = function(constant, power) {
      super$get_python_class()$set_subsampling(
        constant = constant,
        power = power
      )
      invisible(self)
    },

    #' @description Specifies whether the program should display information or
    #'   not.
    #'
    #' @param verbose A boolean specifying whether to display information.
    #'   Defaults to `FALSE`.
    #'
    #' @return The updated \code{\link{CoverComplex}} class itself invisibly.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$set_verbose(FALSE)
    set_verbose = function(verbose = FALSE) {
      super$get_python_class()$set_verbose(verbose)
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()$
    #'   subpopulation(0)
    subpopulation = function(node_id) {
      super$get_python_class()$subpopulation(node_id)
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
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' url <- "https://raw.githubusercontent.com/GUDHI/TDA-tutorial/master/datasets/tore3D_1307.off"
    #' cc <- CoverComplex$new(type = "GIC")
    #' cc$
    #'   read_point_cloud(url)$
    #'   set_graph_from_automatic_rips()$
    #'   set_function_from_coordinate()$
    #'   set_color_from_coordinate()$
    #'   set_resolution_with_interval_number(100)$
    #'   set_cover_from_function()$
    #'   set_automatic_resolution()$
    #'   find_simplices()
    #' withr::with_tempdir({
    #'   cc$write_info()
    #' })
    write_info = function(dir = getwd()) {
      withr::with_dir(dir, {
        super$get_python_class()$write_info()
      })
      invisible(self)
    }
  ),
  private = list(
    m_ComputedPersistenceDiagram = FALSE,
    m_ComputedBootstrapDistribution = FALSE,
    m_ComputedSimplexTree = FALSE,
    m_ComputedSimplicies = FALSE,
    m_IsPointCloudDefinedFromOFF = FALSE,
    m_IsPointCloudDefined = FALSE,
    m_IsGraphDefined = FALSE,
    m_IsCoverDefined = FALSE,
    m_IsCoverDefinedFromFunction = FALSE,
    m_IsFunctionDefined = FALSE,
    m_IsResolutionDefined = FALSE,
    m_IsGraphDefinedFromAutomaticRips = FALSE
  )
)
