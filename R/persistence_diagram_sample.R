#' Persistence Diagram Sample
#'
#' A collection of function to manipulate a persistence diagram sample as an
#' object of class [persistence_diagram_sample]. A [persistence_diagram_sample]
#' is a list of objects of class [persistence_diagram].
#'
#' @param x An object coercible into a [persistence_diagram_sample] object.
#'
#' @return An object of class [persistence_diagram_sample].
#'
#' @name persistence_diagram_sample
NULL

#' @rdname persistence_diagram_sample
#' @export
as_persistence_diagram_sample <- function(x) {
  if (is_persistence_diagram_sample(x)) return(x)
  if (!is.list(x))
    cli::cli_abort("The input argument {.arg x} should be a list.")
  for (d in x) {
    if (!is_persistence_diagram(d))
      cli::cli_abort("All the elements of the input list {.arg x} should be of class {.cls persistence_diagram}.")
  }
  class(x) <- c("persistence_diagram_sample", class(x))
  x
}

#' @rdname persistence_diagram_sample
#' @export
is_persistence_diagram_sample <- function(x) {
  "persistence_diagram_sample" %in% class(x)
}

#' Persistence Diagram Sample Mean
#'
#' Currently computes the Fréchet mean associated with the 2-Wasserstein
#' distance, a.k.a Wasserstein barycenter.
#'
#' @param x An object of class [persistence_diagram_sample].
#' @param initial_center Either an integer value specifying the index of a
#'   persistence diagram in the input list to be used as initial center or an
#'   object of class [persistence_diagram] to be used as initial center.
#'   Defaults to `NULL`, which randomly chooses a center from the input list.
#' @param verbose A boolean specifying whether information should be displayed
#'   into the console. Defaults to `FALSE`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return An object of class [persistence_diagram].
#'
#' @export
#' @examplesIf reticulate::py_module_available("gudhi") && requireNamespace("tibble", quietly = TRUE)
#' dg1 <- as_persistence_diagram(tibble::tibble(birth = 0.2, death = 0.5))
#' dg2 <- as_persistence_diagram(tibble::tibble(birth = 0.2, death = 0.7))
#' dg3 <- as_persistence_diagram(tibble::tibble(
#'   birth = c(0.3, 0.7, 0.2),
#'   death = c(0.6, 0.8, 0.3)
#' ))
#' pdset <- as_persistence_diagram_sample(list(dg1, dg2, dg3))
#' bary <- mean(pdset)
mean.persistence_diagram_sample <- function(x,
                                            initial_center = NULL,
                                            verbose = FALSE,
                                            ...) {
  if (is.null(initial_center)) # sample at random index
    initial_center <- sample.int(length(x), 1L) - 1L
  else if (length(initial_center) == 1L && is.numeric(initial_center)) # use this index
    initial_center <- as.integer(initial_center) - 1L
  else if (is_persistence_diagram(initial_center)) {
    if ("dimension" %in% names(pd)) {
      initial_center$dimension <- NULL
    }
    initial_center <- as.matrix(initial_center)
  } else
    cli::cli_abort("The argument {.arg initial_center} should be either an integer value or a persistence diagram.")

  # massage input to fit expectation from gudhi
  x <- purrr::map(x, \(pd) {
    if ("dimension" %in% names(pd)) {
      pd$dimension <- NULL
    }
    pd
  })
  nm <- names(x[[1]])
  x <- purrr::map(x, as.matrix)

  pd <- gd$wasserstein$barycenter$lagrangian_barycenter(
    pdiagset = x,
    init = initial_center,
    verbose = verbose
  )

  # massage output to fit rgudhi expectation
  colnames(pd) <- nm
  pd <- tibble::as_tibble(pd)
  as_persistence_diagram(pd)
}
