massage_persistence_diagram <- function(x,
                                      dimension = NULL,
                                      max_intervals = 20000) {
  if (is.null(dimension) && !("dimension" %in% names(x)))
    cli::cli_abort("The homology dimension has not been provided. Unable to
                   guess.")
  if (!("dimension" %in% names(x)))
    x$dimension <- dimension
  if (!("death" %in% names(x))) {
    if (!("lifetime" %in% names(x)))
      cli::cli_abort("A persistence diagram should have either a {.var death}
                     column or a {.var lifetime} column.")
    x$death <- x$birth + x$lifetime
  }
  if (!("lifetime" %in% names(x)))
    x$lifetime <- x$death - x$birth
  x |>
    dplyr::group_by(dimension) |>
    dplyr::arrange(dplyr::desc(.data$lifetime)) |>
    dplyr::slice(1:min(dplyr::n(), max_intervals)) |>
    dplyr::ungroup()
}

#' Persistence Diagram
#'
#' A collection of function to manipulate a persistence diagram as an object of
#' class [persistence_diagram]. A [persistence_diagram] is a [tibble::tibble]
#' with a `birth` variable and at least one of `death` or `lifetime` variables.
#'
#' @param x An object coercible into a [persistence_diagram] object.
#'
#' @return An object of class [persistence_diagram].
#'
#' @name persistence_diagram
NULL

#' @rdname persistence_diagram
#' @export
as_persistence_diagram <- function(x) {
  if (is_persistence_diagram(x)) return(x)
  if (!tibble::is_tibble(x))
    cli::cli_abort("Only tibbles are coercible into persistence diagrams.")
  if (!("birth" %in% names(x)))
    cli::cli_abort("The tibble should contain at least the {.var birth}
                   variable.")
  if (!("death" %in% names(x)) && !("lifetime" %in% names(x)))
    cli::cli_abort("The tibble should contain at least one variable among
                   {.var death} and {.var lifetime}.")
  class(x) <- c("persistence_diagram", class(x))
  x
}

#' @rdname persistence_diagram
#' @export
is_persistence_diagram <- function(x) {
  "persistence_diagram" %in% class(x)
}

#' Plot for [`persistence_diagram`] objects
#'
#' This function creates a visualization of a persistence diagram and returns
#' the corresponding [ggplot2::ggplot] object which enable further customization
#' of the plot.
#'
#' @param object An object of class [`persistence_diagram`].
#' @param dimension An integer value specifying the homology dimension to
#'   visualize. Defaults to `NULL` in which case the dimension is retrieved
#'   directly in the [persistence_diagram] object.
#' @param alpha A numeric value between 0 and 1 specifying the transparency of
#'   points and lines in the plot. Defaults to `0.6`.
#' @param max_intervals An integer value specifying the maximal number of
#'   intervals to display. Selected intervals are those with the longest
#'   lifetime. Set it to `0` to see them all. Defaults `20000L`.
#' @param legend A boolean value specifying whether to display the legend about
#'   the homology dimension(s). Defaults to `FALSE`.
#' @param greyblock A boolean value specifying whether to display a grey lower
#'   triangle in the diagram representation for nicer output. Defaults to
#'   `TRUE`.
#' @param n An integer value specifying the number of bins for plotting the
#'   diagram as a density. Defaults to `10L`.
#' @param type A string specifyfing the type of representation. Choices are
#'   `"barcode"`, `"diagram"` or `"density"`. Defaults to `"barcode"`.
#' @param ... Other parameters to be passed on to next methods.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("tibble", quietly = TRUE)
#' pd <- as_persistence_diagram(tibble::tibble(
#'   birth = 0,
#'   death = 1,
#'   dimension = 0
#' ))
#' ggplot2::autoplot(pd)
autoplot.persistence_diagram <- function(object,
                                         dimension = NULL,
                                         alpha = 0.6,
                                         max_intervals = 20000,
                                         legend = FALSE,
                                         greyblock = TRUE,
                                         n = 10L,
                                         type = c("barcode", "diagram", "density"),
                                         ...) {
  type <- rlang::arg_match(type)
  switch(
    type,
    barcode = .plot_persistence_barcode(
      persistence = object,
      dimension = dimension,
      alpha = alpha,
      max_intervals = max_intervals,
      legend = legend
    ),
    diagram = .plot_persistence_diagram(
      persistence = object,
      dimension = dimension,
      alpha = alpha,
      max_intervals = max_intervals,
      legend = legend,
      greyblock = greyblock
    ),
    density = .plot_persistence_density(
      persistence = object,
      dimension = dimension,
      alpha = alpha,
      max_intervals = max_intervals,
      legend = legend,
      greyblock = greyblock,
      n = n
    )
  )
}

#' Plot for [`persistence_diagram`] objects
#'
#' This function creates a visualization of a persistence diagram **without**
#' returning the corresponding [ggplot2::ggplot] object.
#'
#' @param x An object of class [`persistence_diagram`].
#' @inheritParams autoplot.persistence_diagram
#'
#' @return NULL
#'
#' @importFrom graphics plot
#' @export
#' @examplesIf requireNamespace("tibble", quietly = TRUE)
#' pd <- as_persistence_diagram(tibble::tibble(
#'   birth = 0,
#'   death = 1,
#'   dimension = 0
#' ))
#' plot(pd)
plot.persistence_diagram <- function(x,
                                     dimension = NULL,
                                     alpha = 0.6,
                                     max_intervals = 20000,
                                     legend = FALSE,
                                     greyblock = TRUE,
                                     type = c("barcode", "diagram", "density"),
                                     ...) {
  print(autoplot(
    object = x,
    dimension = dimension,
    alpha = alpha,
    max_intervals = max_intervals,
    legend = legend,
    greyblock = greyblock,
    type = type,
    ...
  ))
}

.plot_persistence_barcode <- function(persistence,
                                      dimension = NULL,
                                      alpha = 0.6,
                                      max_intervals = 20000,
                                      legend = FALSE) {
  persistence <- massage_persistence_diagram(
    x = persistence,
    dimension = dimension,
    max_intervals = max_intervals
  )
  persistence |>
    dplyr::arrange(dimension, .data$birth) |>
    dplyr::mutate(id = 1:dplyr::n()) |>
    ggplot2::ggplot(ggplot2::aes(
      y = .data$id,
      xmin = .data$birth,
      xmax = .data$death,
      color = as.factor(.data$dimension))
    ) +
    ggplot2::geom_linerange(alpha = alpha) +
    ggplot2::geom_point(ggplot2::aes(x = .data$birth), alpha = alpha) +
    ggplot2::geom_point(ggplot2::aes(x = .data$death), alpha = alpha) +
    ggplot2::scale_y_continuous(breaks = 1:nrow(persistence)) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "", y = "", color = "Dimension") +
    ggplot2::theme(legend.position = if (legend) "top" else "none")
}

.plot_persistence_diagram <- function(persistence,
                                      dimension = NULL,
                                      alpha = 0.6,
                                      max_intervals = 20000,
                                      legend = FALSE,
                                      greyblock = TRUE) {
  persistence <- massage_persistence_diagram(
    x = persistence,
    dimension = dimension,
    max_intervals = max_intervals
  )
  birth_max <- max(persistence$birth)
  death_max <- max(persistence$death[!is.infinite(persistence$death)])
  p <- ggplot2::ggplot(persistence, ggplot2::aes(.data$birth, .data$death))
  if (greyblock)
    p <- p + ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymax = .data$birth),
      ymin = 0,
      fill = "grey90"
    )
  p +
    ggplot2::geom_point(
      mapping = ggplot2::aes(color = as.factor(.data$dimension)),
      alpha = alpha,
      size = 2
    ) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::coord_fixed(xlim = c(0, death_max), ylim = c(0, death_max)) +
    ggplot2::theme_classic() +
    ggplot2::labs(color = "Dimension") +
    ggplot2::theme(legend.position = if (legend) "top" else "none")
}

.plot_persistence_density <- function(persistence,
                                      dimension = NULL,
                                      alpha = 0.6,
                                      max_intervals = 20000,
                                      legend = FALSE,
                                      greyblock = TRUE,
                                      n = 10) {
  persistence <- massage_persistence_diagram(
    x = persistence,
    dimension = dimension,
    max_intervals = max_intervals
  )
  max_val <- max(persistence$death[!is.infinite(persistence$death)])
  p <- persistence |>
    tibble::add_row(birth = 2 * max_val, death = 2 * max_val) |>
    ggplot2::ggplot(ggplot2::aes(.data$birth, .data$death)) +
    ggplot2::geom_density_2d_filled(breaks = 10^(-n:0), contour_var = "ndensity")
  if (greyblock) p <- p + ggplot2::geom_ribbon(
    mapping = ggplot2::aes(ymax = .data$birth),
    ymin = 0,
    fill = "grey90"
  )
  p <- p +
    ggplot2::coord_fixed(xlim = c(0, max_val), ylim = c(0, max_val)) +
    ggplot2::theme_classic()
  if (!legend || n > 10) p <- p + ggplot2::theme(legend.position = "none")
  p
}
