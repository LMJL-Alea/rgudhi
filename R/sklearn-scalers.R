#' Base Class for Scalers
#'
#' This is the base class for all the scalers available in the
#' [**sklearn.preprocessing**](https://scikit-learn.org/stable/modules/classes.html#module-sklearn.preprocessing)
#' module. The child classes are intended to be used within some GUDHI classes
#' such as [`DiagramScaler`].
#'
#' @keywords internal
BaseScaler <- R6::R6Class(
  classname = "BaseScaler",
  inherit = SKLearnClass
)

#' Scales each feature by its maximum absolute value
#'
#' @description
#' This estimator scales and translates each feature individually such that the
#' maximal absolute value of each feature in the training set will be 1.0. It
#' does not shift/center the data, and thus does not destroy any sparsity.
#'
#' This scaler can also be applied to sparse CSR or CSC matrices.
#'
#' @export
MaxAbsScaler <- R6::R6Class(
  classname = "MaxAbsScaler",
  inherit = BaseScaler,
  public = list(
    #' @description The [MaxAbsScaler] class constructor.
    #'
    #' @param copy A boolean value specifying whether to perform in-place
    #'   scaling and avoid a copy (if the input is already a numpy array).
    #'   Defaults to `TRUE`.
    #'
    #' @return An object of class [MaxAbsScaler].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.preprocessing")
    #' mas <- MaxAbsScaler$new()
    initialize = function(copy = TRUE) {
      super$set_python_class(
        skl_preprocessing$MaxAbsScaler(copy = copy)
      )
    }
  )
)

#' Transforms features by scaling each feature to a given range
#'
#' @description
#' This estimator scales and translates each feature individually such that it
#' is in the given range on the training set, e.g. between zero and one.
#'
#' The transformation is given by:
#' ```
#' X_std = (X - X.min(axis=0)) / (X.max(axis=0) - X.min(axis=0))
#' X_scaled = X_std * (max - min) + min
#' ```
#' where `min, max = feature_range`.
#'
#' This transformation is often used as an alternative to zero mean, unit
#' variance scaling.
#'
#' @export
MinMaxScaler <- R6::R6Class(
  classname = "MinMaxScaler",
  inherit = BaseScaler,
  public = list(
    #' @description The [MinMaxScaler] class constructor.
    #'
    #' @param feature_range A length-2 numeric vector specifying the desired
    #'   range of transformed data. Defaults to `c(0, 1)`.
    #' @param copy A boolean value specifying whether to perform in-place
    #'   scaling and avoid a copy (if the input is already a numpy array).
    #'   Defaults to `TRUE`.
    #' @param clip A boolean value specifying whether to clip transformed values
    #'   of held-out data to provided `feature_range`. Defaults to `FALSE`.
    #'
    #' @return An object of class [MinMaxScaler].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.preprocessing")
    #' mms <- MinMaxScaler$new()
    initialize = function(feature_range = c(0, 1),
                          copy = TRUE,
                          clip = FALSE) {
      feature_range <- reticulate::tuple(feature_range[1], feature_range[2])
      super$set_python_class(
        skl_preprocessing$MinMaxScaler(
          feature_range = feature_range,
          copy = copy,
          clip = clip
        )
      )
    }
  )
)

#' Scales features using statistics that are robust to outliers
#'
#' @description
#' This scaler removes the median and scales the data according to the quantile
#' range (defaults to IQR: Interquartile Range). The IQR is the range between
#' the 1st quartile (25th quantile) and the 3rd quartile (75th quantile).
#'
#' Centering and scaling happen independently on each feature by computing the
#' relevant statistics on the samples in the training set. Median and
#' interquartile range are then stored to be used on later data using the
#' `$transform()` method.
#'
#' Standardization of a dataset is a common requirement for many machine
#' learning estimators. Typically this is done by removing the mean and scaling
#' to unit variance. However, outliers can often influence the sample mean /
#' variance in a negative way. In such cases, the median and the interquartile
#' range often give better results.
#'
#' @export
RobustScaler <- R6::R6Class(
  classname = "RobustScaler",
  inherit = BaseScaler,
  public = list(
    #' @description The [RobustScaler] class constructor.
    #'
    #' @param with_centering A boolean value specifying whether to center the
    #'   data before scaling. This will cause transform to raise an exception
    #'   when attempted on sparse matrices, because centering them entails
    #'   building a dense matrix which in common use cases is likely to be too
    #'   large to fit in memory. Defaults to `TRUE`.
    #' @param with_scaling A boolean value specifying whether to scale the data
    #'   to interquartile range. Defaults to `TRUE`.
    #' @param quantile_range A length-2 numeric vector specifying the quantile
    #'   range used to calculate `scale_`. Defaults to `c(25.0, 75.0)`.
    #' @param copy A boolean value specifying whether to perform in-place
    #'   scaling and avoid a copy (if the input is already a numpy array).
    #'   Defaults to `TRUE`.
    #' @param unit_variance A boolean value specifying whether to scale data so
    #'   that normally distributed features have a variance of 1. In general, if
    #'   the difference between the x-values of \eqn{q_{\max}} and
    #'   \eqn{q_{\min}} for a standard normal distribution is greater than 1,
    #'   the data set will be scaled down. If less than 1, the data set will be
    #'   scaled up. Defaults to `FALSE`.
    #'
    #' @return An object of class [RobustScaler].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.preprocessing")
    #' rs <- RobustScaler$new()
    initialize = function(with_centering = TRUE,
                          with_scaling = TRUE,
                          quantile_range = c(25.0, 75.0),
                          copy = TRUE,
                          unit_variance = FALSE) {
      quantile_range <- reticulate::tuple(quantile_range[1], quantile_range[2])
      super$set_python_class(
        skl_preprocessing$RobustScaler(
          with_centering = with_centering,
          with_scaling = with_scaling,
          quantile_range = quantile_range,
          copy = copy,
          unit_variance = unit_variance
        )
      )
    }
  )
)

#' Standardizes features by removing the mean and scaling to unit variance
#'
#' @description
#' The standard score of a sample x is calculated as:
#' \deqn{z = \frac{(x - u)}{s}}
#' where \eqn{u} is the mean of the training samples or 0 if `with_mean =
#' FALSE`, and \eqn{s} is the standard deviation of the training samples or 1 if
#' `with_std = FALSE`.
#'
#' Standardization of a dataset is a common requirement for many machine
#' learning estimators: they might behave badly if the individual features do
#' not more or less look like standard normally distributed data (e.g. Gaussian
#' with 0 mean and unit variance).
#'
#' For instance many elements used in the objective function of a learning
#' algorithm (such as the RBF kernel of Support Vector Machines or the L1 and L2
#' regularizers of linear models) assume that all features are centered around 0
#' and have variance in the same order. If a feature has a variance that is
#' orders of magnitude larger than others, it might dominate the objective
#' function and make the estimator unable to learn from other features correctly
#' as expected.
#'
#' This scaler can also be applied to sparse CSR or CSC matrices by passing
#' `with_mean = FALSE` to avoid breaking the sparsity structure of the data.
#'
#' @export
StandardScaler <- R6::R6Class(
  classname = "StandardScaler",
  inherit = BaseScaler,
  public = list(
    #' @description The [StandardScaler] class constructor.
    #'
    #' @param copy A boolean value specifying whether to perform in-place
    #'   scaling and avoid a copy (if the input is already a numpy array).
    #'   Defaults to `TRUE`.
    #' @param with_mean A boolean value specifying whether to center the data
    #'   before scaling. This does not work (and will raise an exception) when
    #'   attempted on sparse matrices, because centering them entails building a
    #'   dense matrix which in common use cases is likely to be too large to fit
    #'   in memory. Defaults to `TRUE`.
    #' @param with_std A boolean value specifying whether to scale the data to
    #'   unit variance (or equivalently, unit standard deviation). Defaults to
    #'   `TRUE`.
    #'
    #' @return An object of class [StandardScaler].
    #'
    #' @examplesIf reticulate::py_module_available("sklearn.preprocessing")
    #' ss <- StandardScaler$new()
    initialize = function(copy = TRUE,
                          with_mean = TRUE,
                          with_std = TRUE) {
      super$set_python_class(
        skl_preprocessing$StandardScaler(
          copy = copy,
          with_mean = with_mean,
          with_std = with_std
        )
      )
    }
  )
)
