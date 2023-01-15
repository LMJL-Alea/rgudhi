#' Kernel Representation Step
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
KernelRepresentationStep <- R6::R6Class(
  classname = "KernelRepresentationStep",
  inherit = SKLearnClass,
  public = list(
    #' @description Applies the class on a single persistence diagram and
    #'   outputs the result.
    #'
    #' @return A numeric value storing the kernel-induced inner product between
    #'   the two input diagrams.
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
    #'   n_\mathrm{in}} storing the kernel-induced inner product between the
    #'   \eqn{n_\mathrm{out}} persistence diagrams passed to the `$transform()`
    #'   method and the \eqn{n_\mathrm{in}} persistence diagrams passed to the
    #'   `$fit()` method.
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
    #' @return A numeric matrix of shape \eqn{n \times n} storing the
    #'   kernel-induced inner product between the \eqn{n} persistence diagrams
    #'   passed to both the `$fit()` and `$transform()` methods.
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

#' Kernel Representation: Persistence Fisher Kernel
#'
#' @description Computes the persistence Fisher kernel matrix from a list of
#'   persistence diagrams. The persistence Fisher kernel is computed by
#'   exponentiating the corresponding persistence Fisher distance with a
#'   Gaussian kernel. See
#'   papers.nips.cc/paper/8205-persistence-fisher-kernel-a-riemannian-manifold-kernel-for-persistence-diagrams
#'   for more details.
#'
#' @author Mathieu Carrière
#' @export
PersistenceFisherKernel <- R6::R6Class(
  classname = "PersistenceFisherKernel",
  inherit = KernelRepresentationStep,
  public = list(
    #' @description The [`PersistenceFisherKernel`] constructor.
    #'
    #' @param bandwidth_fisher A numeric value specifying the bandwidth of the
    #'   Gaussian kernel used to turn persistence diagrams into probability
    #'   distributions by the `PersistenceFisherDistance` class. Defaults to
    #'   `1.0`.
    #' @param bandwidth A numeric value specifying the bandwidth of the Gaussian
    #'   kernel applied to the persistence Fisher distance. Defaults to `1.0`.
    #' @param kernel_approx A Python class specifying the kernel approximation
    #'   class used to speed up computation. Defaults to `NULL`. Common kernel
    #'   approximations classes can be found in the **scikit-learn** library
    #'   (such as `RBFSampler` for instance).
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1`.
    #'
    #' @return An object of class [`PersistenceFisherKernel`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' pfk <- PersistenceFisherKernel$new()
    #' pfk$apply(dgm, dgm)
    #' pfk$fit_transform(list(dgm))
    initialize = function(bandwidth_fisher = 1.0,
                          bandwidth = 1.0,
                          kernel_approx = NULL,
                          n_jobs = 1) {
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$PersistenceFisherKernel(
          bandwidth_fisher = bandwidth_fisher,
          bandwidth = bandwidth,
          kernel_approx = kernel_approx,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Kernel Representation: Persistence Scale-Space Kernel
#'
#' @description Computes the persistence scale space kernel matrix from a list
#'   of persistence diagrams. The persistence scale space kernel is computed by
#'   adding the symmetric to the diagonal of each point in each persistence
#'   diagram, with negative weight, and then convolving the points with a
#'   Gaussian kernel. See
#'   https://www.cv-foundation.org/openaccess/content_cvpr_2015/papers/Reininghaus_A_Stable_Multi-Scale_2015_CVPR_paper.pdf
#'   for more details.
#'
#' @author Mathieu Carrière
#' @export
PersistenceScaleSpaceKernel <- R6::R6Class(
  classname = "PersistenceScaleSpaceKernel",
  inherit = KernelRepresentationStep,
  public = list(
    #' @description The [`PersistenceScaleSpaceKernel`] constructor.
    #'
    #' @param bandwidth A numeric value specifying the bandwidth of the Gaussian
    #'   kernel with which persistence diagrams will be convolved. Defaults to
    #'   `1.0`.
    #' @param kernel_approx A Python class specifying the kernel approximation
    #'   class used to speed up computation. Defaults to `NULL`. Common kernel
    #'   approximations classes can be found in the **scikit-learn** library
    #'   (such as `RBFSampler` for instance).
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1`.
    #'
    #' @return An object of class [`PersistenceScaleSpaceKernel`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' pssk <- PersistenceScaleSpaceKernel$new()
    #' pssk$apply(dgm, dgm)
    #' pssk$fit_transform(list(dgm))
    initialize = function(bandwidth = 1.0,
                          kernel_approx = NULL,
                          n_jobs = 1) {
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$PersistenceScaleSpaceKernel(
          bandwidth = bandwidth,
          kernel_approx = kernel_approx,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Kernel Representation: Persistence Weighted Gaussian Kernel
#'
#' @description Computes the persistence weighted Gaussian kernel matrix from a
#'   list of persistence diagrams. The persistence weighted Gaussian kernel is
#'   computed by convolving the persistence diagram points with weighted
#'   Gaussian kernels. See http://proceedings.mlr.press/v48/kusano16.html for
#'   more details.
#'
#' @author Mathieu Carrière
#' @export
PersistenceWeightedGaussianKernel <- R6::R6Class(
  classname = "PersistenceWeightedGaussianKernel",
  inherit = KernelRepresentationStep,
  public = list(
    #' @description The [`PersistenceWeightedGaussianKernel`] constructor.
    #'
    #' @param bandwidth A numeric value specifying the bandwidth of the Gaussian
    #'   kernel with which persistence diagrams will be convolved. Defaults to
    #'   `1.0`.
    #' @param weight A function or a formula coercible into a function via
    #'   [rlang::as_function()] specifying the weight function for the
    #'   persistence diagram points. Defaults to the constant function `~ 1`.
    #'   This function must be defined on 2D points, i.e. lists or arrays of the
    #'   form \eqn{[p_x,p_y]}.
    #' @param kernel_approx A Python class specifying the kernel approximation
    #'   class used to speed up computation. Defaults to `NULL`. Common kernel
    #'   approximations classes can be found in the **scikit-learn** library
    #'   (such as `RBFSampler` for instance).
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1`.
    #'
    #' @return An object of class [`PersistenceWeightedGaussianKernel`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' pwgk <- PersistenceWeightedGaussianKernel$new()
    #' pwgk$apply(dgm, dgm)
    #' pwgk$fit_transform(list(dgm))
    initialize = function(bandwidth = 1.0,
                          weight = ~ 1,
                          kernel_approx = NULL,
                          n_jobs = 1) {
      weight <- rlang::as_function(weight)
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$PersistenceWeightedGaussianKernel(
          bandwidth = bandwidth,
          weight = weight,
          kernel_approx = kernel_approx,
          n_jobs = n_jobs
        )
      )
    }
  )
)

#' Kernel Representation: Persistence Sliced Wasserstein Kernel
#'
#' @description Computes the sliced Wasserstein kernel matrix from a list of
#'   persistence diagrams. The sliced Wasserstein kernel is computed by
#'   exponentiating the corresponding sliced Wasserstein distance with a
#'   Gaussian kernel. See http://proceedings.mlr.press/v70/carriere17a.html for
#'   more details.
#'
#' @author Mathieu Carrière
#' @export
PersistenceSlicedWassersteinKernel <- R6::R6Class(
  classname = "PersistenceSlicedWassersteinKernel",
  inherit = KernelRepresentationStep,
  public = list(
    #' @description The [`PersistenceSlicedWassersteinKernel`] constructor.
    #'
    #' @param num_directions An integer value specifying the number of lines
    #'   evenly sampled from \eqn{[-\pi/2,\pi/2]} in order to approximate and
    #'   speed up the kernel computation. Defaults to `10L`.
    #' @param bandwidth A numeric value specifying the bandwidth of the Gaussian
    #'   kernel with which persistence diagrams will be convolved. Defaults to
    #'   `1.0`.
    #' @param n_jobs An integer value specifying the number of jobs to use for
    #'   the computation. Defaults to `1`.
    #'
    #' @return An object of class [`PersistenceSlicedWassersteinKernel`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' pswk <- PersistenceSlicedWassersteinKernel$new()
    #' pswk$apply(dgm, dgm)
    #' pswk$fit_transform(list(dgm))
    initialize = function(num_directions = 10,
                          bandwidth = 1.0,
                          n_jobs = 1) {
      num_directions <- as.integer(num_directions)
      n_jobs <- as.integer(n_jobs)
      super$set_python_class(
        gd$representations$SlicedWassersteinKernel(
          num_directions = num_directions,
          bandwidth = bandwidth,
          n_jobs = n_jobs
        )
      )
    }
  )
)
