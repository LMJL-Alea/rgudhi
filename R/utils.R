read_off_file <- function(off) {
  if (fs::path_ext(off) != "off")
    cli::cli_abort("The input file should have extension {.code .off}.")
  header <- readr::read_delim(
    file = off,
    delim = " ",
    n_max = 1,
    col_names = FALSE,
    col_types = "c"
  )
  if (header != "OFF")
    cli::cli_abort("The input file is not an OFF file.")
  header <- as.numeric(readr::read_delim(
    file = off,
    delim = " ",
    n_max = 1,
    skip = 1,
    col_names = FALSE,
    col_types = "ddd"
  ))
  if (header[2] != 0 || header[3] != 0)
    cli::cli_abort("The input file is not an OFF file.")
  res <- readr::read_delim(
    file = off,
    delim = " ",
    skip = 2,
    col_names = FALSE,
    col_types = "ddd"
  )
  if (nrow(res) != header[1])
    cli::cli_abort("The number of points does not match the one reported in the header.")
  res <- purrr::array_tree(res, margin = 1)
  purrr::map(res, purrr::set_names, nm = NULL)
}
