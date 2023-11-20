utils::globalVariables("workfile")

read_off_file <- function(off_file) {
  if (fs::path_ext(off_file) != "off")
    cli::cli_abort("The input file should have extension {.code .off}.")
  if (substr(off_file, 1, 5) == "https") {
    withr::with_tempfile("workfile", {
      download_file(off_file, workfile)
      gd$read_points_from_off_file(workfile)
    })
  } else
    gd$read_points_from_off_file(off_file)
}

download_file <- function(input_file, output_file) {
  if (substr(input_file, 1, 5) != "https")
    cli::cli_abort("Input file {.file {input_file}} is not a valid HTTPS url.")
  curl::curl_download(input_file, destfile = output_file)
}

#' Circular Sequence Generation
#'
#' Generates a sequence of 2D points evenly spaced on the unit circle.
#'
#' @param n An integer value specifying the number of points in the sequence.
#'
#' @return A [base::list] of length-2 numeric vectors storing 2D points evenly
#'   spaced on the unit circle.
#'
#' @export
#' @examples
#' seq_circle(10)
seq_circle <- function(n) {
  if (n == 0)
    return(list())
  purrr::map(
    seq(0, 2 * pi, len = n + 1)[1:n],
    ~ c(cos(.x), sin(.x))
  )
}

capture_extra_params <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots))
    cli::cli_abort("All extra-arguments should be named arguments.")
  dots
}

#' Convert Persistence Diagrams to Ragged Tensors
#'
#' @param diagrams A list of numeric matrices of dimension \eqn{n \times 2}
#'  specifying a sample of \eqn{n} persistence diagrams.
#'
#' @return A ragged tensor of shape \eqn{n \times \mathrm{None} \times 2}.
#'
#' @export
#' @examples
#' dgm <- list(
#'  matrix(c(0, 1, 0, 2), ncol = 2, byrow = TRUE),
#'  matrix(c(0, 1, 0, 2, 0, 3), ncol = 2, byrow = TRUE)
#' )
#' dgm
#' to_ragged_tensor(dgm)
to_ragged_tensor <- function(diagrams) {
  diagrams |>
    purrr::map(as.matrix) |>
    purrr::map(\(dgm) purrr::array_tree(dgm, margin = 1)) |>
    tf$ragged$constant(ragged_rank = 1L, dtype = tf$float32)
}

to_diagrams <- function(ragged_tensor) {
  ragged_tensor$to_list() |>
    purrr::map(\(dgm) do.call(rbind, dgm)) |>
    purrr::map(\(dgm) `colnames<-`(dgm, c("birth", "death"))) |>
    purrr::map(tibble::as_tibble) |>
    purrr::map(as_persistence_diagram) |>
    as_persistence_diagram_sample()
}

doc_perslay_diagrams_param <- function() {
  "A ragged tensor of shape \\eqn{n \\times \\mathrm{None} \\times 2} specifying
  a sample of \\eqn{n} persistence diagrams. The second dimension is ragged
  because the number of points in each diagram may vary."
}

doc_perslay_phi_samples_param <- function() {
  "A numeric vector specifying the grid elements on which to evaluate the phi
  function."
}

doc_perslay_phi_dots_param <- function() {
  "A named list providing extra arguments for compatibility with the TensorFlow
  API. Not used here."
}
