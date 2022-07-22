N <- 1307
withr::with_seed(1234, {
  runif(N) %>%
    write_lines("inst/extdata/color_file.txt")
})
withr::with_seed(1234, {
  runif(N) %>%
    write_lines("inst/extdata/function_file.txt")
})
fs::file_delete("inst/extdata/cover_file.txt")
withr::with_seed(1234, {
  rpois(N, 1) %>%
    `+`(1) %>%
    map(~ sample(0:7, .x)) %>%
    walk(
      .f = write,
      file = "inst/extdata/cover_file.txt",
      append = TRUE,
      ncolumns = 100
    )
})
