#' Vector Representation Step
#'
#' @param diag A 2-column [tibble::tibble] specifying a persistence diagram.
#' @param X A list of 2-column [tibble::tibble]s specifying a sample of
#'   persistence diagrams.
#' @param y An integer vector specifying persistence diagram labels (unused for
#'   now).
#'
#' @author Mathieu Carrière
#' @keywords internal
VectorRepresentationStep <- R6::R6Class(
  classname = "VectorRepresentationStep",
  inherit = SKLearnClass,
  public = list(
    #' @description Applies the class on a single persistence diagram and
    #'   outputs the result.
    #'
    #' @return A [tibble::tibble] storing the requested vector representation of
    #'   the persistence diagram in a table suitable for visualization.
    apply = function(diag) {
      diag |>
        as.matrix() |>
        super$apply() |>
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
    #' @return A list of [tibble::tibble]s storing the requested vector
    #'   representations of the persistence diagrams in a table suitable for
    #'   visualization.
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
    #' @return A list of [tibble::tibble]s storing the requested vector
    #'   representations of the persistence diagrams in a table suitable for
    #'   visualization.
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
      repr_values <- if (length(dim(x)) == 1) x else purrr::array_tree(x, margin = 2)
      py_cls <- super$get_python_class()
      if ("grid_" %in% names(py_cls))
        grid <- as.numeric(py_cls$grid_)
      else if ("sample_range" %in% names(py_cls) && !anyNA(py_cls$sample_range))
        grid <- seq(
          from = py_cls$sample_range[1],
          to = py_cls$sample_range[2],
          length.out = py_cls$resolution
        )
      else if ("im_range" %in% names(py_cls))
        grid <- purrr::cross_df(list(
          X = seq(
            from = py_cls$im_range[1],
            to = py_cls$im_range[3],
            length.out = py_cls$resolution[1]
          ),
          Y = seq(
            from = py_cls$im_range[2],
            to = py_cls$im_range[4],
            length.out = py_cls$resolution[2]
          )
        ))
      else
        grid <- 1:length(repr_values)

      if ("num_landscapes" %in% names(py_cls)) {
        num_landscapes <- py_cls$num_landscapes
        out <- tibble::tibble(
          Grid = rep(grid, times = num_landscapes),
          LandscapeId = rep(1:num_landscapes, each = py_cls$resolution),
          Value = repr_values
        )
      }
      else if ("im_range" %in% names(py_cls)) {
        out <- grid
        out$Value <- repr_values
      }
      else
        out <- tibble::tibble(Grid = grid, Value = repr_values)
      out
    }
  )
)

#' Vector Representation: Atol
#'
#' @description Computes measure vectorization (e.g. point clouds, persistence
#'   diagrams, etc.) after a quantisation step according to the Atol algorithm
#'   \insertCite{royer2021atol}{rgudhi}.
#'
#' ## References
#' \insertCited{}
#'
#' @author Mathieu Carrière
#'
#' @export
Atol <- R6::R6Class(
  classname = "Atol",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`Atol`] constructor.
    #'
    #' @param quantiser An object of class [`BaseClustering`] specifying
    #'   any clustering algorithm from the
    #'   [**sklearn.cluster**](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.cluster)
    #'   module. It will be fitted when the `$fit()` method is called.
    #' @param weighting_method A string specifying the constant generic function
    #'   for weighting the measure points. Choices are either `"cloud"` or
    #'   `"iidproba"`. Defaults to `"cloud"`, i.e. the measure is seen as a
    #'   point cloud. This will have no impact if weights are provided along
    #'   with measures all the way, i.e. at `$fit()` and `$transform()` calls,
    #'   through the optional argument `sample_weight`.
    #' @param contrast A string specifying the constant function for evaluating
    #'   proximity of a measure with respect to centers. Choices are either
    #'   `"gaussian"` or `"laplacian"` or `"indicator"`. Defaults to
    #'   `"gaussian"` (see page 3 in
    #'   \insertCite{royer2021atol;textual}{rgudhi}).
    #'
    #' @return An object of class [`Atol`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' km <- KMeans$new(n_clusters = 2, random_state = 202006)
    #' vr <- Atol$new(quantiser = km)
    #' # vr$apply(dgm) # TODO: needs fix in python
    #' vr$fit_transform(list(dgm))
    initialize = function(quantiser,
                          weighting_method = c("cloud", "iidproba"),
                          contrast = c("gaussian", "laplacian", "indicator")) {
      quantiser <- quantiser$get_python_class()
      weighting_method <- rlang::arg_match(weighting_method)
      contrast <- rlang::arg_match(contrast)
      super$set_python_class(
        gd$representations$Atol(
          quantiser = quantiser,
          weighting_method = weighting_method,
          contrast = contrast
        )
      )
    }
  )
)

#' Vector Representation: Betti Curve
#'
#' @description Computes Betti curves from persistence diagrams. There are
#'   several modes of operation: with a given resolution (with or without a
#'   `sample_range`), with a predefined grid, and with none of the previous.
#'   With a predefined grid, the class computes the Betti numbers at those grid
#'   points. Without a predefined grid, if the resolution is set to `NULL`, it
#'   can be fit to a list of persistence diagrams and produce a grid that
#'   consists of (at least) the filtration values at which at least one of those
#'   persistence diagrams changes Betti numbers, and then compute the Betti
#'   numbers at those grid points. In the latter mode, the exact Betti curve is
#'   computed for the entire real line. Otherwise, if the resolution is given,
#'   the Betti curve is obtained by sampling evenly using either the given
#'   `sample_range` or based on the persistence diagrams.
#'
#' @author Mathieu Carrière
#' @export
BettiCurve <- R6::R6Class(
  classname = "BettiCurve",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`BettiCurve`] constructor.
    #'
    #' @param resolution An integer value specifying the number of sample for
    #'   the piecewise constant function. Defaults to `100L`.
    #' @param sample_range A length-2 numeric vector specifying the minimum and
    #'   maximum of the piecewise constant function domain, of the form
    #'   \eqn{[x_{\min}, x_{\max}]}. Defaults to `rep(NA, 2)`. It is the
    #'   interval on which samples will be drawn evenly. If one of the values is
    #'   `NA`, it can be computed from the persistence diagrams with the
    #'   `$fit()` method.
    #' @param predefined_grid A numeric vector specifying a predefined grid of
    #'   points at which to compute the Betti curves. Must be strictly ordered.
    #'   Infinities are ok. If set to `NULL` (default), and resolution is given,
    #'   the grid will be uniform from \eqn{x_{\min}} to \eqn{x_{\max}} in
    #'   `resolution` steps, otherwise a grid will be computed that captures all
    #'   changes in Betti numbers in the provided data.
    #'
    #' @return An object of class [`BettiCurve`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' bc <- BettiCurve$new()
    #' bc$apply(dgm)
    #' bc$fit_transform(list(dgm))
    initialize = function(resolution = 100,
                          sample_range = rep(NA, 2),
                          predefined_grid = NULL) {
      resolution <- as.integer(resolution)
      super$set_python_class(
        gd$representations$BettiCurve(
          resolution = resolution,
          sample_range = sample_range,
          predefined_grid = predefined_grid
        )
      )
    }
  )
)

#' Vector Representation: Complex Polynomial
#'
#' @description Computes complex polynomials from a list of persistence
#'   diagrams. The persistence diagram points are seen as the roots of some
#'   complex polynomial, whose coefficients are returned in a complex vector.
#'   See https://link.springer.com/chapter/10.1007%2F978-3-319-23231-7_27 for
#'   more details.
#'
#' @author Mathieu Carrière
#' @export
ComplexPolynomial <- R6::R6Class(
  classname = "ComplexPolynomial",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`ComplexPolynomial`] constructor.
    #'
    #' @param polynomial_type A string specifying the Type of complex polynomial
    #'   that is going to be computed (explained in
    #'   https://link.springer.com/chapter/10.1007%2F978-3-319-23231-7_27).
    #'   Choices are `c("R", "S", "T")`. Defaults to `"R"`.
    #' @param threshold An integer value specifying the number of coefficients.
    #'   This is the dimension of the complex vector of coefficients, i.e. the
    #'   number of coefficients corresponding to the largest degree terms of the
    #'   polynomial. If `-1`, this threshold is computed from the list of
    #'   persistence diagrams by considering the one with the largest number of
    #'   points and using the dimension of its corresponding complex vector of
    #'   coefficients as threshold. Defaults to `10L`.
    #'
    #' @return An object of class [`ComplexPolynomial`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' cp <- ComplexPolynomial$new()
    #' cp$apply(dgm)
    #' cp$fit_transform(list(dgm))
    initialize = function(polynomial_type = c("R", "S", "T"), threshold = 10) {
      polynomial_type <- rlang::arg_match(polynomial_type)
      threshold <- as.integer(threshold)
      super$set_python_class(
        gd$representations$ComplexPolynomial(
          polynomial_type = polynomial_type,
          threshold = threshold
        )
      )
    }
  )
)

#' Vector Representation: Entropy
#'
#' @description Computes persistence entropy. Persistence entropy is a statistic
#'   for persistence diagrams inspired from Shannon entropy. This statistic can
#'   also be used to compute a feature vector, called the entropy summary
#'   function. See https://arxiv.org/pdf/1803.08304.pdf for more details. Note
#'   that a previous implementation was contributed by Manuel Soriano-Trigueros.
#'
#' @author Mathieu Carrière
#' @export
Entropy <- R6::R6Class(
  classname = "Entropy",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`Entropy`] constructor.
    #'
    #' @param mode A string specifying which entropy to compute: either
    #'   `"scalar"` for computing the entropy statistic, or `"vector"` for
    #'   computing the entropy summary function. Defaults to `"scalar"`.
    #' @param normalized A boolean value specifying whether to normalize the
    #'   entropy summary function. Defaults to `TRUE`. Used only if `mode ==
    #'   "vector"`.
    #' @param resolution An integer value specifying the grid size for the
    #'   entropy summary function. Defaults to `100L`. Used only if `mode ==
    #'   "vector"`.
    #' @param sample_range A length-2 numeric vector specifying the domain for
    #'   the entropy summary function, of the form \eqn{[x_{\min}, x_{\max}]}.
    #'   Defaults to `rep(NA, 2)`. It is the interval on which samples will be
    #'   drawn evenly. If one of the values is `NA`, it can be computed from the
    #'   persistence diagrams with the `$fit()` method. Used only if `mode ==
    #'   "vector"`.
    #'
    #' @return An object of class [`Entropy`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' ent <- Entropy$new()
    #' ent$apply(dgm)
    #' ent$fit_transform(list(dgm))
    initialize = function(mode = c("scalar", "vector"),
                          normalized = TRUE,
                          resolution = 100,
                          sample_range = rep(NA_real_, 2)) {
      mode <- rlang::arg_match(mode)
      resolution <- as.integer(resolution)
      super$set_python_class(
        gd$representations$Entropy(
          mode = mode,
          normalized = normalized,
          resolution = resolution,
          sample_range = sample_range
        )
      )
    }
  )
)

#' Vector Representation: Landscape
#'
#' @description Computes persistence landscapes from a list of persistence
#'   diagrams. A persistence landscape is a collection of 1D piecewise-linear
#'   functions computed from the rank function associated to the persistence
#'   diagram. These piecewise-linear functions are then sampled evenly on a
#'   given range and the corresponding vectors of samples are concatenated and
#'   returned. See http://jmlr.org/papers/v16/bubenik15a.html for more details.
#'
#' @author Mathieu Carrière
#' @export
Landscape <- R6::R6Class(
  classname = "Landscape",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`Landscape`] constructor.
    #'
    #' @param num_landscapes An integer value specifying the number of piecewise
    #'   linear functions to output. Defaults to `5L`.
    #' @param resolution An integer value specifying the grid size for the
    #'   landscapes. Defaults to `100L`.
    #' @param sample_range A length-2 numeric vector specifying the domain for
    #'   the entropy summary function, of the form \eqn{[x_{\min}, x_{\max}]}.
    #'   Defaults to `rep(NA, 2)`. It is the interval on which samples will be
    #'   drawn evenly. If one of the values is `NA`, it can be computed from the
    #'   persistence diagrams with the `$fit()` method.
    #'
    #' @return An object of class [`Landscape`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' lds <- Landscape$new()
    #' lds$apply(dgm)
    #' lds$fit_transform(list(dgm))
    initialize = function(num_landscapes = 5,
                          resolution = 100,
                          sample_range = rep(NA_real_, 2)) {
      num_landscapes <- as.integer(num_landscapes)
      resolution <- as.integer(resolution)
      super$set_python_class(
        gd$representations$Landscape(
          num_landscapes = num_landscapes,
          resolution = resolution,
          sample_range = sample_range
        )
      )
    }
  )
)

#' Vector Representation: Persistence Image
#'
#' @description Computes persistence images from a list of persistence diagrams.
#'   A persistence image is a 2D function computed from a persistence diagram by
#'   convolving the diagram points with a weighted Gaussian kernel. The plane is
#'   then discretized into an image with pixels, which is flattened and returned
#'   as a vector. See http://jmlr.org/papers/v18/16-337.html for more details.
#'
#' @author Mathieu Carrière
#' @export
PersistenceImage <- R6::R6Class(
  classname = "PersistenceImage",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`PersistenceImage`] constructor.
    #'
    #' @param bandwidth A numeric value specifying the bandwidth of the Gaussian
    #'   kernel. Defaults to `1.0`.
    #' @param weight A function or a formula coercible into a function via
    #'   [rlang::as_function()] specifying the weight function for the
    #'   persistence diagram points. Defaults to the constant function `~ 1`.
    #'   This function must be defined on 2D points, i.e. lists or arrays of the
    #'   form \eqn{[p_x,p_y]}.
    #' @param resolution An length-1 integer vector specifying the size (in
    #'   pixels) of the persistence image. Defaults to `rep(20L, 2)`.
    #' @param im_range A length-4 numeric vector specifying the two-dimensional
    #'   domain for the persistence image, of the form \eqn{[x_{\min}, y_{\min},
    #'   x_{\max}, y_{\max}]}. Defaults to `rep(NA, 4)`. If one of the values is
    #'   `NA`, it can be computed from the persistence diagrams with the
    #'   `$fit()` method.
    #'
    #' @return An object of class [`PersistenceImage`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' pei <- PersistenceImage$new()
    #' pei$apply(dgm)
    #' pei$fit_transform(list(dgm))
    initialize = function(bandwidth = 1.0,
                          weight = ~ 1,
                          resolution = c(20, 20),
                          im_range = rep(NA_real_, 4)) {
      weight <- rlang::as_function(weight)
      resolution <- as.integer(resolution)
      super$set_python_class(
        gd$representations$PersistenceImage(
          bandwidth = bandwidth,
          weight = weight,
          resolution = resolution,
          im_range = im_range
        )
      )
    }
  )
)

#' Vector Representation: Silhouette
#'
#' @description Computes persistence silhouettes from a list of persistence
#'   diagrams. A persistence silhouette is computed by taking a weighted average
#'   of the collection of 1D piecewise-linear functions given by the persistence
#'   landscapes, and then by evenly sampling this average on a given range.
#'   Finally, the corresponding vector of samples is returned. See
#'   https://arxiv.org/abs/1312.0308 for more details.
#'
#' @author Mathieu Carrière
#' @export
Silhouette <- R6::R6Class(
  classname = "Silhouette",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`Silhouette`] constructor.
    #'
    #' @param weight A function or a formula coercible into a function via
    #'   [rlang::as_function()] specifying the weight function for the
    #'   persistence diagram points. Defaults to the constant function `~ 1`.
    #'   This function must be defined on 2D points, i.e. lists or arrays of the
    #'   form \eqn{[p_x,p_y]}.
    #' @param resolution An length-1 integer vector specifying the size (in
    #'   pixels) of the persistence image. Defaults to `rep(20L, 2)`.
    #' @param sample_range A length-2 numeric vector specifying the domain for
    #'   the entropy summary function, of the form \eqn{[x_{\min}, x_{\max}]}.
    #'   Defaults to `rep(NA, 2)`. It is the interval on which samples will be
    #'   drawn evenly. If one of the values is `NA`, it can be computed from the
    #'   persistence diagrams with the `$fit()` method.
    #'
    #' @return An object of class [`Silhouette`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' sil <- Silhouette$new()
    #' sil$apply(dgm) # TO DO: fix gd because it does not set sample_range automatically
    #' sil$fit_transform(list(dgm))
    initialize = function(weight = ~ 1,
                          resolution = 100,
                          sample_range = rep(NA_real_, 2)) {
      weight <- rlang::as_function(weight)
      resolution <- as.integer(resolution)
      super$set_python_class(
        gd$representations$Silhouette(
          weight = weight,
          resolution = resolution,
          sample_range = sample_range
        )
      )
    }
  )
)

#' Vector Representation: Topological Vector
#'
#' @description Computes topological vectors from a list of persistence
#'   diagrams. The topological vector associated to a persistence diagram is the
#'   sorted vector of a slight modification of the pairwise distances between
#'   the persistence diagram points. See
#'   https://diglib.eg.org/handle/10.1111/cgf12692 for more details.
#'
#' @author Mathieu Carrière
#' @export
TopologicalVector <- R6::R6Class(
  classname = "TopologicalVector",
  inherit = VectorRepresentationStep,
  public = list(
    #' @description The [`TopologicalVector`] constructor.
    #'
    #' @param threshold An integer value specifying the number of distances to
    #'   keep. Defaults to `10L`. This is the dimension of the topological
    #'   vector. If `-1`, this threshold is computed from the list of
    #'   persistence diagrams by considering the one with the largest number of
    #'   points and using the dimension of its corresponding topological vector
    #'   as threshold.
    #'
    #' @return An object of class [`TopologicalVector`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' X <- seq_circle(10)
    #' ac <- AlphaComplex$new(points = X)
    #' st <- ac$create_simplex_tree()
    #' dgm <- st$compute_persistence()$persistence_intervals_in_dimension(0)
    #' ds <- DiagramSelector$new(use = TRUE)
    #' dgm <- ds$apply(dgm)
    #' tv <- TopologicalVector$new()
    #' tv$apply(dgm)
    #' tv$fit_transform(list(dgm))
    initialize = function(threshold = 10) {
      threshold <- as.integer(threshold)
      super$set_python_class(
        gd$representations$TopologicalVector(
          threshold = threshold
        )
      )
    }
  )
)
