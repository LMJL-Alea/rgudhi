#' Clustering: Tomato
#'
#' @description This clustering algorithm needs a neighborhood graph on the
#'   points, and an estimation of the density at each point. A few possible
#'   graph constructions and density estimators are provided for convenience,
#'   but it is perfectly natural to provide your own.
#'
#' @author Marc Glisse
#' @export
Tomato <- R6::R6Class(
  classname = "Tomato",
  inherit = PythonClass,
  public = list(
    #' @description The [`Tomato`] constructor.
    #'
    #' @param graph_type A string specifying the method to compute the
    #'   neighboring graph. Choices are `"knn"`, `"radius"` or `"manual"`.
    #'   Defaults to `"knn"`.
    #' @param density_type A string specifying the choice of density estimator.
    #'   Choicea are `"logDTM"`, `"DTM"`, `"logKDE"` or `"manual"`. When you
    #'   have many points, `"KDE"` and `"logKDE"` tend to be slower. Defaults to
    #'   `"logDTM"`
    #' @param n_clusters An integer value specifying the number of clusters.
    #'   Defaults to `NULL`, i.e. no merging occurs and we get the maximal
    #'   number of clusters.
    #' @param merge_threshold A numeric value specifying the minimum prominence
    #'   of a cluster so it doesn’t get merged. Defaults to `NULL`, i.e. no
    #'   merging occurs and we get the maximal number of clusters.
    #' @param ... Extra parameters passed to `KNearestNeighbors` and
    #'   `DTMDensity`.
    #'
    #' @return An object of class [`Tomato`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(100)
    #' cl <- Tomato$new()
    #' cl$fit_predict(X)
    #' cl$set_n_clusters(2)
    #' cl$get_labels()
    initialize = function(graph_type = c("knn", "radius", "manual"),
                          density_type = c("logDTM", "DTM", "logKDE", "KDE", "manual"),
                          n_clusters = NULL,
                          merge_threshold = NULL,
                          ...) {
      dots <- capture_extra_params(...)
      dots$graph_type <- rlang::arg_match(graph_type)
      dots$density_type <- rlang::arg_match(density_type)
      if (!is.null(n_clusters)) n_clusters <- as.integer(n_clusters)
      dots$n_clusters <- n_clusters
      dots$merge_threshold <- merge_threshold
      super$set_python_class(do.call(gd$clustering$tomato$Tomato, dots))
    },

    #' @description Runs the Tomato algorithm on the provided data.
    #'
    #' @param X Either a numeric matrix specifying the coordinates (in column)
    #'   of each point (in row) or a **full** distance matrix if `metric ==
    #'   "precomputed"` or a list of neighbors for each point if `graph_type ==
    #'   "manual"`. The number of points is currently limited to about 2
    #'   billion.
    #' @param y  Not used, present here for API consistency with
    #'   **scikit-learn** by convention.
    #' @param weights A numeric vector specifying a density estimate at each
    #'   point. Used only if `density_type == "manual"`.
    #'
    #' @return The updated [`Tomato`] class itself invisibly.
    fit = function(X, y = NULL, weights = NULL) {
      super$get_python_class()$fit(X = X, y = y, weights = weights)
      invisible(self)
    },

    #' @description Runs the Tomato algorithm on the provided data **and**
    #'   returns the class memberships.
    #'
    #' @param X Either a numeric matrix specifying the coordinates (in column)
    #'   of each point (in row) or a **full** distance matrix if `metric ==
    #'   "precomputed"` or a list of neighbors for each point if `graph_type ==
    #'   "manual"`. The number of points is currently limited to about 2
    #'   billion.
    #' @param y  Not used, present here for API consistency with
    #'   **scikit-learn** by convention.
    #' @param weights A numeric vector specifying a density estimate at each
    #'   point. Used only if `density_type == "manual"`.
    #'
    #' @return An integer vector storing the class memberships.
    fit_predict = function(X, y = NULL, weights = NULL) {
      as.integer(super$get_python_class()$fit_predict(
        X = X,
        y = y,
        weights = weights
      ))
    },

    #' @description Sets the number of clusters which automatically adjusts
    #'   class memberships.
    #'
    #' @param n_clusters An integer value specifying the number of clusters.
    #'
    #' @return The updated [`Tomato`] class itself invisibly.
    set_n_clusters = function(n_clusters) {
      py <- super$get_python_class()
      py$n_clusters_ <- as.integer(n_clusters)
      invisible(self)
    },

    #' @description Gets the number of clusters.
    #'
    #' @return The number of clusters.
    get_n_clusters = function() {
      py <- super$get_python_class()
      py$n_clusters_
    },

    #' @description Sets the threshold for merging clusters which automatically
    #'   adjusts class memberships.
    #'
    #' @param merge_threshold A numeric value specifying the threshold for
    #'   merging clusters.
    #'
    #' @return The updated [`Tomato`] class itself invisibly.
    set_merge_threshold = function(merge_threshold) {
      py <- super$get_python_class()
      py$merge_threshold_ <- merge_threshold
      invisible(self)
    },

    #' @description Gets the threshold for merging clusters.
    #'
    #' @return The threshold for merging clusters.
    get_merge_threshold = function() {
      py <- super$get_python_class()
      py$merge_threshold_
    },

    #' @description Gets the class memberships.
    #'
    #' @return An integer vector storing the class memberships.
    get_labels = function() {
      py <- super$get_python_class()
      as.integer(py$labels_)
    },

    #' @description Computes the persistence diagram of the merge tree of the
    #'   initial clusters. This is a convenient graphical tool to help decide
    #'   how many clusters we want.
    plot_diagram = function() {
      py <- super$get_python_class()
      dgm <- py$diagram_
      grid_min <- min(dgm)
      grid_max <- max(dgm)
      dgm |>
        `colnames<-`(c("x", "y")) |>
        tibble::as_tibble() |>
        ggplot2::ggplot(ggplot2::aes(x, y)) +
        ggplot2::geom_point(color = "red") +
        ggplot2::geom_point(
          data = tibble::tibble(x = grid_max, y = grid_min),
          color = "darkgreen"
        ) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linewidth = 1) +
        ggplot2::coord_equal(
          xlim = c(grid_min, grid_max),
          ylim = c(grid_min, grid_max)
        ) +
        ggplot2::theme_bw()
    }
  )
)
