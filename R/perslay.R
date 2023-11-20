#' Perslay: Base Class
#' @keywords internal
PerslayBaseClass <- R6::R6Class(
  classname = "PerslayBaseClass",
  inherit = PythonClass,
  public = list(
    #' @description Creates the variables of the layer (optional, for subclass
    #'   implementers).
    #'
    #' This is a method that implementers of subclasses of `Layer` or `Model`
    #' can override if they need a state-creation step in-between layer
    #' instantiation and layer call. It is invoked automatically before the
    #' first execution of `call()`.
    #'
    #' This is typically used to create the weights of `Layer` subclasses (at
    #' the discretion of the subclass implementer).
    #'
    #' @param input_shape Instance of `TensorShape`, or list of instances of
    #'  `TensorShape` if the layer expects a list of inputs (one instance per
    #'  input).
    #'
    #' @return The class itself invisibly.
    build = function(input_shape) {
      super$get_python_class()$build(input_shape)
      invisible(self)
    },

    #' @description Apply the layer to an input, or list of inputs, and return
    #'  the result.
    #'
    #' @param diagrams `r doc_perslay_diagrams_param()`
    #'
    #' @return Either a ragged tensor or a tensor storing the result of the
    #'  layer.
    call = function(diagrams) {
      super$get_python_class()$call(diagrams)
    }
  )
)

#' Perslay: Main Class
#'
#' @description This is a TensorFlow layer for vectorizing persistence diagrams
#'   in a differentiable way within a neural network. This function implements
#'   the PersLay equation \insertCite{carriere2020perslay}{rgudhi}.
#'
#' ## References
#' \insertCited{}
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
Perslay <- R6::R6Class(
  classname = "Perslay",
  inherit = PerslayBaseClass,
  public = list(
    #' @description The [`Perslay`] constructor.
    #'
    #' @param weight A weight function for the persistence diagram points. Can
    #'   be either an object of class [`GridPerslayWeight`],
    #'   [`GaussianMixturePerslayWeight`], [`PowerPerslayWeight`] or a custom
    #'   TensorFlow function that takes persistence diagrams as argument
    #'   (represented as an \eqn{n \times \mathrm{None} \times 2} ragged tensor,
    #'   where \eqn{n} is the number of diagrams).
    #' @param phi A transformation function for the persistence diagram points.
    #'   Can be either an object of class [`GaussianPerslayPhi`],
    #'   [`TentPerslayPhi`], [`FlatPerslayPhi`] or a custom TensorFlow class
    #'   (that can have trainable parameters) with a method `call()` that takes
    #'   persistence diagrams as argument (represented as an \eqn{n \times
    #'   \mathrm{None} \times 2} ragged tensor, where \eqn{n} is the number of
    #'   diagrams).
    #' @param perm_op A permutation invariant function, such as
    #'   `tf$math$reduce_sum`, `tf$math$reduce_mean`, `tf$math$reduce_max`,
    #'   `tf$math$reduce_min` or a custom TensorFlow function that takes two
    #'   arguments: a tensor and an axis on which to apply the permutation
    #'   invariant operation. If `perm_op` is the string `"topk"` (where \eqn{k}
    #'   is a number), this function will be computed as `tf$math$top_k` with
    #'   integer parameter `k`.
    #' @param rho A postprocessing function that is applied after the
    #'   permutation invariant operation. Can be any TensorFlow layer.
    #' @param ... A named list providing extra arguments for compatibility with
    #'   the TensorFlow API. Not used here.
    #'
    #' @return An object of class [`Perslay`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' phi <- FlatPerslayPhi$new(
    #'   samples = c(1:13, 12:1),
    #'   theta = 0.1
    #' )
    #' weight <- GaussianMixturePerslayWeight$new(
    #'   gaussians = matrix(c(0, 0, 1, 1), nrow = 4, ncol = 1)
    #' )
    #' perm_op <- tf$math$reduce_sum
    #' rho <- tf$identity
    #'
    #' perslay <- Perslay$new(
    #'   phi = phi,
    #'   weight = weight,
    #'   perm_op = perm_op,
    #'   rho = rho
    #' )
    initialize = function(weight,
                          phi,
                          perm_op,
                          rho,
                          ...) {
      super$set_python_class(
        tfInterface$perslay$Perslay(
          weight = weight$get_python_class(),
          phi = phi$get_python_class(),
          perm_op = perm_op,
          rho = rho,
          ...
        )
      )
    },

    #' @description Applies Perslay on a ragged tensor containing a list of
    #'   persistence diagrams.
    #'
    #' @param diagrams `r doc_perslay_diagrams_param()`
    #'
    #' @return A tensor storing the vectorizations of the persistence diagrams.
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' phi <- FlatPerslayPhi$new(
    #'   samples = c(1:13, 12:1),
    #'   theta = 0.1
    #' )
    #' weight <- GaussianMixturePerslayWeight$new(
    #'   gaussians = matrix(c(0, 0, 1, 1), nrow = 4, ncol = 1)
    #' )
    #' perm_op <- tf$math$reduce_sum
    #' rho <- tf$identity
    #'
    #' perslay <- Perslay$new(
    #'   phi = phi,
    #'   weight = weight,
    #'   perm_op = perm_op,
    #'   rho = rho
    #' )
    #'
    #' vectors <- perslay$call(dgms)
    call = function(diagrams) {
      super$call(diagrams)
    }
  )
)

#' Perslay: Base Perslay Weight Class
#' @description This is a base class for computing a differentiable weight
#'  function for persistence diagram points.
#' @keywords internal
BasePerslayWeightClass <- R6::R6Class(
  classname = "BasePerslayWeightClass",
  inherit = PerslayBaseClass,
  public = list(
    #' @description Apply the layer to an input, or list of inputs, and return
    #'   the result.
    #'
    #' @param diagrams `r doc_perslay_diagrams_param()`
    #'
    #' @return A ragged tensor storing the weights of the points in the \eqn{n}
    #'   persistence diagrams. The second dimension is ragged since persistence
    #'   diagrams can have different numbers of points.
    call = function(diagrams) {
      super$call(diagrams)
    }
  )
)

#' Perslay: Gaussian Mixture Perslay Weight
#'
#' @description This is a class for computing a differentiable weight function
#'   for persistence diagram points. This function is defined from a mixture of
#'   Gaussian functions.
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
GaussianMixturePerslayWeight <- R6::R6Class(
  classname = "GaussianMixturePerslayWeight",
  inherit = BasePerslayWeightClass,
  public = list(
    #' @description The [`GaussianMixturePerslayWeight`] constructor.
    #'
    #' @param gaussians A numeric matrix of size \eqn{4 \times n} specifying a
    #'   two-dimensional Gaussian distribution for each of the \eqn{n} diagrams.
    #'   Each column must store the Gaussian parameters in the following order:
    #'   \eqn{\mu_x, \mu_y, \sigma_x, \sigma_y}.
    #' @param ... A named list providing extra arguments for compatibility with
    #'   the TensorFlow API. Not used here.
    #'
    #' @return An object of class [`GaussianMixturePerslayWeight`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' weight <- GaussianMixturePerslayWeight$new(
    #'   gaussians = matrix(c(0, 0, 1, 1), nrow = 4, ncol = 1)
    #' )
    #' weight$call(dgms)
    initialize = function(gaussians, ...) {
      gaussians <- gaussians |>
        as.matrix() |>
        tf$constant(dtype = tf$float32)
      super$set_python_class(
        tfInterface$perslay$GaussianMixturePerslayWeight(
          gaussians = gaussians,
          ...
        )
      )
    }
  )
)

#' Perslay: Grid Perslay Weight
#'
#' @description This is a class for computing a differentiable weight function
#'   for persistence diagram points. This function is defined from an array that
#'   contains its values on a 2D grid.
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
GridPerslayWeight <- R6::R6Class(
  classname = "GridPerslayWeight",
  inherit = BasePerslayWeightClass,
  public = list(
    #' @description The [`GridPerslayWeight`] constructor.
    #'
    #' @param grid A numeric matrix of shape \eqn{n \times n} specifying the
    #'   grid of values.
    #' @param grid_bnds A numeric matrix of shape \eqn{2 \times 2} specifying
    #'   the boundaries of the grid. It should be of the form
    #'   \eqn{\begin{bmatrix} x_{min} & x_{max} \\ y_{min} & y_{max}
    #'   \end{bmatrix}}.
    #' @param ... A named list providing extra arguments for compatibility with
    #'   the TensorFlow API. Not used here.
    #'
    #' @return An object of class [`GridPerslayWeight`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' weight <- GridPerslayWeight$new(
    #'   grid = matrix(c(1:13, 12:1), 5, 5),
    #'   grid_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5))
    #' )
    #' weight$call(dgms)
    initialize = function(grid, grid_bnds, ...) {
      grid <- grid |>
        as.matrix() |>
        tf$constant(dtype = tf$float32)
      grid_bnds <- grid_bnds |>
        as.matrix() |>
        tf$constant(dtype = tf$float32)
      super$set_python_class(
        tfInterface$perslay$GridPerslayWeight(
          grid = grid,
          grid_bnds = grid_bnds,
          ...
        )
      )
    }
  )
)

#' Perslay: Power Perslay Weight
#'
#' @description This is a class for computing a differentiable weight function
#'   for persistence diagram points. This function is defined as a constant
#'   multiplied by the distance to the diagonal of the persistence diagram point
#'   raised to some power.
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
PowerPerslayWeight <- R6::R6Class(
  classname = "PowerPerslayWeight",
  inherit = PerslayBaseClass,
  public = list(
    #' @description The [`PowerPerslayWeight`] constructor.
    #'
    #' @param constant A numeric value specifying the constant of the
    #'   transformation.
    #' @param power A numeric value specifying the power of the transformation
    #'   which will be applied to the distance to the diagonal.
    #' @param ... A named list providing extra arguments for compatibility with
    #'   the TensorFlow API. Not used here.
    #'
    #' @return An object of class [`PowerPerslayWeight`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' weight <- PowerPerslayWeight$new(
    #'   constant = 1,
    #'   power = 0
    #' )
    #' weight$call(dgms)
    initialize = function(constant, power, ...) {
      constant <- as.numeric(constant)
      power <- as.numeric(power)
      super$set_python_class(
        tfInterface$perslay$PowerPerslayWeight(
          constant = constant,
          power = power,
          ...
        )
      )
    }
  )
)

#' Perslay: Base Perslay Phi Class
#' @keywords internal
BasePerslayPhiClass <- R6::R6Class(
  classname = "BasePerslayPhiClass",
  inherit = PerslayBaseClass,
  public = list(
    #' @description Apply the layer to an input, or list of inputs, and return
    #'   the result.
    #'
    #' @param diagrams `r doc_perslay_diagrams_param()`
    #'
    #' @return A ragged tensor storing the evaluations on the 1D grid of the 1D
    #'   phi functions corresponding to the persistence diagram points. The
    #'   second dimension is ragged since persistence diagrams can have
    #'   different numbers of points.
    call = function(diagrams) {
      super$call(diagrams)
    }
  )
)

#' Perslay: Flat Perslay Phi Class
#'
#' @description
#' This is a class for computing a transformation function for persistence
#' diagram points. This function turns persistence diagram points into 1D
#' constant functions (that evaluate to half of the bar length on the bar
#' corresponding to the point and zero elsewhere), that are then evaluated on a
#' regular 1D grid.
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
FlatPerslayPhi <- R6::R6Class(
  classname = "FlatPerslayPhi",
  inherit = BasePerslayPhiClass,
  public = list(
    #' @description The [`FlatPerslayPhi`] constructor.
    #'
    #' @param samples `r doc_perslay_phi_samples_param()`
    #' @param theta A numeric value specifying the sigmoid parameter used to
    #'   approximate the constant function with a differentiable sigmoid
    #'   function. The bigger `theta`, the closer to a constant function the
    #'   output will be.
    #' @param ... `r doc_perslay_phi_dots_param()`
    #'
    #' @return An object of class [`FlatPerslayPhi`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' phi <- FlatPerslayPhi$new(
    #'   samples = c(1:13, 12:1),
    #'   theta = 0.1
    #' )
    #' phi$call(dgms)
    initialize = function(samples, theta, ...) {
      samples <- as.numeric(samples)
      theta <- as.numeric(theta)
      super$set_python_class(
        tfInterface$perslay$FlatPerslayPhi(
          samples = samples,
          theta = theta,
          ...
        )
      )
    }
  )
)

#' Perslay: Gaussian Perslay Phi Class
#'
#' @description This is a class for computing a transformation function for
#'   persistence diagram points. This function turns persistence diagram points
#'   into 2D Gaussian functions centered on the points, that are then evaluated
#'   on a regular 2D grid.
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
GaussianPerslayPhi <- R6::R6Class(
  classname = "GaussianPerslayPhi",
  inherit = BasePerslayPhiClass,
  public = list(
    #' @description The [`GaussianPerslayPhi`] constructor.
    #'
    #' @param image_size A length-2 integer vector specifying the size of the 2D
    #'   grid on which the phi function must be evaluated.
    #' @param image_bnds An integer matrix of shape \eqn{2 \times 2} specifying
    #'   the boundaries of the grid on which the phi function must be evaluated.
    #'   It must be of the form \eqn{\begin{bmatrix} x_{min} & x_{max} \\
    #'   y_{min} & y_{max} \end{bmatrix}}.
    #' @param variance A numeric value specifying the variance of the Gaussian
    #'   functions.
    #' @param ... `r doc_perslay_phi_dots_param()`
    #'
    #' @return An object of class [`GaussianPerslayPhi`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' phi <- GaussianPerslayPhi$new(
    #'   image_size = c(5, 5),
    #'   image_bnds = rbind(c(-.5, 1.5), c(-.5, 1.5)),
    #'   variance = .1
    #' )
    #' phi$call(dgms)
    initialize = function(image_size, image_bnds, variance, ...) {
      tf <- tfInterface$perslay$tf
      image_size <- as.integer(image_size)
      image_bnds <- image_bnds |>
        as.matrix() |>
        tf$constant(dtype = tf$float32)
      variance <- as.numeric(variance)
      super$set_python_class(
        tfInterface$perslay$GaussianPerslayPhi(
          image_size = image_size,
          image_bnds = image_bnds,
          variance = variance,
          ...
        )
      )
    }
  )
)

#' Perslay: Tent Perslay Phi Class
#'
#' @description This is a class for computing a transformation function for
#'   persistence diagram points. This function turns persistence diagram points
#'   into 1D tent functions (linearly increasing on the first half of the bar
#'   corresponding to the point from zero to half of the bar length, linearly
#'   decreasing on the second half and zero elsewhere) centered on the points,
#'   that are then evaluated on a regular 1D grid.
#'
#' @author Mathieu Carrière, Martin Royer, Gard Spreemann, Wojciech Reise
#' @export
TentPerslayPhi <- R6::R6Class(
  classname = "TentPerslayPhi",
  inherit = BasePerslayPhiClass,
  public = list(
    #' @description The [`TentPerslayPhi`] constructor.
    #'
    #' @param samples `r doc_perslay_phi_samples_param()`
    #' @param ... `r doc_perslay_phi_dots_param()`
    #'
    #' @return An object of class [`TentPerslayPhi`].
    #'
    #' @examplesIf reticulate::py_module_available("gudhi")
    #' diagrams <- as_persistence_diagram_sample(list(
    #'   as_persistence_diagram(tibble::tibble(
    #'     birth = c(0, 1, 3, 6),
    #'     death = c(4, 2, 8, 8)
    #'   ))
    #' ))
    #' ds <- DiagramScaler$new(
    #'   use = TRUE,
    #'   scalers = list(list(c(0, 1), MinMaxScaler$new()))
    #' )
    #' dgms <- diagrams |>
    #'   ds$fit_transform() |>
    #'   to_ragged_tensor()
    #'
    #' phi <- TentPerslayPhi$new(
    #'   samples = c(1:13, 12:1)
    #' )
    #' phi$call(dgms)
    initialize = function(samples, ...) {
      samples <- as.numeric(samples)
      super$set_python_class(
        tfInterface$perslay$TentPerslayPhi(
          samples = samples,
          ...
        )
      )
    }
  )
)
