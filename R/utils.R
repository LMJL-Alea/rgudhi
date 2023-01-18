utils::globalVariables("tf")

read_off_file <- function(off_file) {
  if (fs::path_ext(off_file) != "off")
    cli::cli_abort("The input file should have extension {.code .off}.")
  if (substr(off_file, 1, 5) == "https") {
    withr::with_tempfile("tf", {
      download_file(off_file, tf)
      gd$read_points_from_off_file(tf)
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
