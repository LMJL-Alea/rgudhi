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
    #' @return A numeric value storing the persistence Fisher kernel value.
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
    #' @examples
    #' n <- 10
    #' X <- lapply(
    #'   seq(0, 2 * pi, len = n + 1)[1:n],
    #'   function(.x) c(cos(.x), sin(.x))
    #' )
    #' if (reticulate::py_module_available("gudhi")) {
    #'   ac <- AlphaComplex$new(points = X)
    #'   st <- ac$create_simplex_tree()
    #'   dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #'   ds <- DiagramSelector$new()
    #'   dgm <- ds$apply(dgm)
    #'   pfk <- PersistenceFisherKernel$new()
    #'   pfk$apply(dgm, dgm)
    #'   pfk$fit_transform(list(dgm))
    #' }
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
