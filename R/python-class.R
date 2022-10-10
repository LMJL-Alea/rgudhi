PythonClass <- R6::R6Class(
  classname = "PythonClass",
  public = list(
    set_python_class = function(py_class) {
      private$python_class = py_class
    },
    get_python_class = function() {
      private$python_class
    }
  ),
  private = list(
    python_class = NULL
  )
)
