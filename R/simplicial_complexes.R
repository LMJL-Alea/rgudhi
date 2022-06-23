#' Alpha Complex Class
#'
#' AlphaComplex is a simplicial complex constructed from the finite cells of a
#' Delaunay Triangulation.
#'
#' The filtration value of each simplex is computed as the square of the
#' circumradius of the simplex if the circumsphere is empty (the simplex is then
#' said to be Gabriel), and as the minimum of the filtration values of the
#' codimension 1 cofaces that make it not Gabriel otherwise. All simplices that
#' have a filtration value strictly greater than a given alpha squared value are
#' not inserted into the complex.
#'
#' @param points Either a nxd matrix or a length-n list of d-dimensional vectors
#'   or a file with extension `.off`.
#' @param precision A string specifying the alpha complex precision. Can be one
#'   of `"fast"`, `"safe"` or `"exact"`. Defaults to `"safe"`.
#'
#' @export
#' @examples
#' n <- 20
#' X_list <- replicate(n, runif(2), simplify = FALSE)
#' X_matrix <- Reduce(rbind, X_list, init = numeric())
#' ac_matrix <- alpha_complex(points = X_matrix)
#' st_matrix <- ac_matrix$create_simplex_tree()
#' dgm_matrix <- st_matrix$persistence()
#' plot_persistence_diagram(dgm_matrix)
#' ac_list <- alpha_complex(points = X_list)
#' st_list <- ac_list$create_simplex_tree()
#' dgm_list <- st_list$persistence()
#' plot_persistence_diagram(dgm_list)
alpha_complex <- function(points, precision = "safe") {
  if (inherits(points, "matrix") || inherits(points, "list"))
    gd$AlphaComplex(points = points)
  else if (is.character(points) && fs::path_ext(points) == "off")
    gd$AlphaComplex(off_file = points, precision = precision)
  else
    cli::cli_abort("{.code points} must be either a matrix or a list or an OFF file.")
}

#' @export
plot_persistence_diagram <- function(diagram) {
  gd$plot_persistence_diagram(diagram)
  plt$show()
}
