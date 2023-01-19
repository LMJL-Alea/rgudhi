#' Metric Step
#'
#' @param diag1 A 2-column [tibble::tibble] specifying a persistence diagram.
#' @param diag2 A 2-column [tibble::tibble] specifying a persistence diagram.
#' @param X A list of 2-column [tibble::tibble]s specifying a sample of
#'   persistence diagrams.
#' @param y An integer vector specifying persistence diagram labels (unused for
#'   now).
#'
#' @author Mathieu Carrière
#' @keywords internal
MetricStep <- R6::R6Class(
  classname = "MetricStep",
  inherit = SKLearnClass,
  public = list(
    #' @description Applies the class on a single persistence diagram and
    #'   outputs the result.
    #'
    #' @return A numeric value storing the distance between the two input
    #'   diagrams.
    apply = function(diag1, diag2) {
      diag1 |>
        as.matrix() |>
        super$apply(as.matrix(diag2)) |>
        private$convert_output()
    },

    #' @description Fits the class on a sample of persistence diagrams.
    #'
    #' @return The class itself invisibly.
    fit = function(X, y = NULL) {
      X |>
        purrr::map(as.matrix) |>
        super$fit(y)
      invisible(self)
    },

    #' @description Applies the class on a sample of persistence diagrams.
    #'
    #' @return A numeric matrix of shape \eqn{n_\mathrm{out} \times
    #'   n_\mathrm{in}} storing the distances between the \eqn{n_\mathrm{out}}
    #'   persistence diagrams passed to the `$transform()` method and the
    #'   \eqn{n_\mathrm{in}} persistence diagrams passed to the `$fit()` method.
    transform = function(X) {
      X |>
        purrr::map(as.matrix) |>
        super$transform() |>
        private$convert_output()
    },

    #' @description Applies sequentially the `$fit()` and `$transform()` methods
    #'   on a sample of persistence diagrams in a more efficient way than
    #'   calling them directly.
    #'
    #' @return A numeric matrix of shape \eqn{n \times n} storing the distance
    #'   between the \eqn{n} persistence diagrams passed to both the `$fit()`
    #'   and `$transform()` methods.
    fit_transform = function(X, y = NULL) {
      X <- purrr::map(X, as.matrix)
      super$fit(X, y)
      X |>
        super$transform() |>
        private$convert_output()
    }
  ),
  private = list(
    convert_output = function(x) {
      if (is.list(x)) purrr::map(x, private$convert_output)
      x
    }
  )
)

#' Metrics: Bottleneck Distance
#'
#' @description Computes the bottleneck distance matrix from a list of
#'   persistence diagrams.
#'
#' @author Mathieu Carrière
#' @export
BottleneckDistance <- R6::R6Class(
  classname = "BottleneckDistance",
  inherit = MetricStep,
  public = list(
    #' @description The [`BottleneckDistance`] constructor.
    #'
    #' @param epsilon A numeric value specifying the absolute (additive) error
    #'   tolerated on the distance. Defaults to `NULL`, in which case the
    #'   smallest positive float is used.
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1L`.
    #'
    #' @return An object of class [`BottleneckDistance`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' dis <- BottleneckDistance$new()
    #' dis$apply(dgm, dgm)
    #' dis$fit_transform(list(dgm))
    initialize = function(epsilon = NULL, n_jobs = 1) {
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$BottleneckDistance(
          epsilon = epsilon,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Metrics: Persistence Fisher Distance
#'
#' @description Computes the persistence Fisher distance matrix from a list of
#'   persistence diagrams. The persistence Fisher distance is obtained by
#'   computing the original Fisher distance between the probability
#'   distributions associated to the persistence diagrams given by convolving
#'   them with a Gaussian kernel. See
#'   http://papers.nips.cc/paper/8205-persistence-fisher-kernel-a-riemannian-manifold-kernel-for-persistence-diagrams
#'   for more details.
#'
#' @author Mathieu Carrière
#' @export
PersistenceFisherDistance <- R6::R6Class(
  classname = "PersistenceFisherDistance",
  inherit = MetricStep,
  public = list(
    #' @description The [`PersistenceFisherDistance`] constructor.
    #'
    #' @param bandwidth A numeric value specifying the bandwidth of the Gaussian
    #'   kernel applied to the persistence Fisher distance. Defaults to `1.0`.
    #' @param kernel_approx A Python class specifying the kernel approximation
    #'   class used to speed up computation. Defaults to `NULL`. Common kernel
    #'   approximations classes can be found in the **scikit-learn** library
    #'   (such as `RBFSampler` for instance).
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1L`.
    #'
    #' @return An object of class [`PersistenceFisherDistance`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' dis <- PersistenceFisherDistance$new()
    #' dis$apply(dgm, dgm)
    #' dis$fit_transform(list(dgm))
    initialize = function(bandwidth = 1.0, kernel_approx = NULL, n_jobs = 1) {
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$PersistenceFisherDistance(
          bandwidth = bandwidth,
          kernel_approx = kernel_approx,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Metrics: Sliced Wasserstein Distance
#'
#' @description Computes the sliced Wasserstein distance matrix from a list of
#'   persistence diagrams. The Sliced Wasserstein distance is computed by
#'   projecting the persistence diagrams onto lines, comparing the projections
#'   with the 1-norm, and finally integrating over all possible lines. See
#'   http://proceedings.mlr.press/v70/carriere17a.html for more details.
#'
#' @author Mathieu Carrière
#' @export
SlicedWassersteinDistance <- R6::R6Class(
  classname = "SlicedWassersteinDistance",
  inherit = MetricStep,
  public = list(
    #' @description The [`SlicedWassersteinDistance`] constructor.
    #'
    #' @param num_directions An integer value specifying the number of lines
    #'   evenly sampled from \eqn{[-\pi/2,\pi/2]} in order to approximate and
    #'   speed up the kernel computation. Defaults to `10L`.
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1L`.
    #'
    #' @return An object of class [`SlicedWassersteinDistance`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' dis <- SlicedWassersteinDistance$new()
    #' dis$apply(dgm, dgm)
    #' dis$fit_transform(list(dgm))
    initialize = function(num_directions = 10, n_jobs = 1) {
      num_directions <- as.integer(num_directions)
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$SlicedWassersteinDistance(
          num_directions = num_directions,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Metrics: Wasserstein Distance
#'
#' @description Computes the Wasserstein distance matrix from a list of
#'   persistence diagrams.
#'
#' @author Mathieu Carrière
#' @export
WassersteinDistance <- R6::R6Class(
  classname = "WassersteinDistance",
  inherit = MetricStep,
  public = list(
    #' @description The [`WassersteinDistance`] constructor.
    #'
    #' @param order An integer value specifying the exponent of the Wasserstein
    #'   distance. Defaults to `1.0`.
    #' @param internal_p An integer value specifying the ground metric on the
    #'   (upper-half) plane (i.e. the norm \eqn{\ell_p} in \eqn{R^2}). Defaults
    #'   to `Inf`.
    #' @param mode A string specifying the method for computing the Wasserstein
    #'   distance. Choices are either `"pot"` or `"hera"`. Defaults to `"hera"`.
    #' @param delta A numeric value specifying the relative error
    #'   \eqn{1+\delta}. Defaults to `0.01`. Used only if `mode == "hera"`.
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1L`.
    #'
    #' @return An object of class [`WassersteinDistance`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' dis <- WassersteinDistance$new()
    #' dis$apply(dgm, dgm)
    #' dis$fit_transform(list(dgm))
    initialize = function(order = 1,
                          internal_p = Inf,
                          mode = c("hera", "pot"),
                          delta = 0.01,
                          n_jobs = 1) {
      order <- as.integer(order)
      internal_p <- if (is.infinite(internal_p)) Inf else as.integer(internal_p)
      mode <- rlang::arg_match(mode)
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$WassersteinDistance(
          order = order,
          internal_p = internal_p,
          mode = mode,
          delta = delta,
          n_jobs = n_jobs
        )
      )
    }
  )
)
