SKLearnClass <- R6::R6Class(
  classname = "SKLearnClass",
  inherit = PythonClass,
  public = list(
    apply = function(...) {
      super$get_python_class()(...)
    },
    set_params = function(...) {
      super$get_python_class()$set_params(...)
      invisible(self)
    },
    get_params = function(deep = TRUE) {
      super$get_python_class()$get_params(deep = deep)
    },
    fit = function(X, y = NULL) {
      super$get_python_class()$fit(X, y)
      invisible(self)
    },
    transform = function(X) {
      super$get_python_class()$transform(X)
    },
    fit_transform = function(X, y = NULL) {
      super$get_python_class()$fit_transform(X, y)
    }
  )
)
