check_persistence_diagram <- function(x, dimension = NULL) {
  if (is.null(dimension) && !("dimension" %in% names(x)))
    cli::cli_abort("The homology dimension has not been provided. Unable to guess.")
  if (!("dimension" %in% names(x)))
    x$dimension <- dimension
  if (!("death" %in% names(x))) {
    if (!("lifetime" %in% names(x)))
      cli::cli_abort("A persistence diagram should have either a {.var death} column or a {.var lifetime} column.")
    x$death <- x$birth + x$lifetime
  }
  x
}

plot_diagram <- function(x, dimension = NULL) {
  x <- check_persistence_diagram(x, dimension = dimension)
  birth_max <- max(x$birth)
  death_max <- max(x$death[!is.infinite(x$death)])
  x |>
    ggplot(aes(birth, death, color = as.factor(dimension))) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    coord_fixed(xlim = c(0, birth_max), ylim = c(0, death_max)) +
    theme_linedraw() +
    labs(
      color = "Dimension"
    ) +
    theme(legend.position = "top")
}

plot_barcodes <- function(x, dimension = NULL) {
  x <- check_persistence_diagram(x, dimension = dimension)
  birth_max <- max(x$birth)
  death_max <- max(x$death[!is.infinite(x$death)])
  x |>
    arrange(dimension, birth) |>
    mutate(id = 1:n()) |>
    ggplot(aes(y = id, xmin = birth, xmax = death, color = as.factor(dimension))) +
    geom_linerange() +
    geom_point(aes(x = birth)) +
    geom_point(aes(x = death)) +
    scale_y_continuous(breaks = 1:nrow(x)) +
    theme_linedraw() +
    labs(
      x = "", y = "",
      color = "Dimension"
    ) +
    theme(legend.position = "top")
}
