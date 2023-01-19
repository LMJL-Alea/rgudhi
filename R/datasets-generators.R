#' Sampling on the Sphere
#'
#' The function [sphere()] enables uniform sampling of random *i.i.d.* points on
#' a \eqn{(d-1)}-sphere in \eqn{R^d}. The user should provide the number of
#' points `n_samples` to be generated on the sphere and the ambient dimension
#' `ambient_dim`. The radius of the sphere is optional and is equal to `1` by
#' default. Only random points generation is currently available.
#'
#' @param n_samples An integer value specifying the sample size.
#' @param ambient_dim An integer value specifying the dimension of the ambient
#'   space.
#' @param radius A numeric value specifying the radius of the sphere. Defaults
#'   to `1.0`.
#'
#' @return A numeric array of shape \eqn{n_\mathrm{samples} \times
#'   \mathrm{ambient\_dim}} storing `n_samples` points uniformly sampled on the
#'   sphere of dimension `ambient_dim - 1`.
#'
#' @export
#' @examplesIf reticulate::py_module_available("gudhi")
#' sphere(10, 2)
sphere <- function(n_samples, ambient_dim, radius = 1.0) {
  n_samples <- as.integer(n_samples)
  ambient_dim <- as.integer(ambient_dim)
  gd$datasets$generators$points$sphere(
    n_samples = n_samples,
    ambient_dim = ambient_dim,
    radius = radius
  )
}

#' Sampling on the Torus
#'
#' The user should provide the number of points `n_samples` to be generated on
#' the torus and the dimension `dim` of the torus on which points would be
#' generated in \eqn{R^{2 \mathrm{dim}}}. The `sample` argument is optional and
#' is set to `"random"` by default. The generated points are
#' returned as an array of shape \eqn{n_\mathrm{samples} \times R^{2
#' \mathrm{dim}}}.
#'
#' @param n_samples An integer value specifying the sample size.
#' @param dim An integer value specifying the dimension \eqn{R^{2 \mathrm{dim}}}
#'   of the torus.
#' @param sample A string specifying the sampling type. Choices are `"random"`
#'   or `"grid"`. Defaults to `"random"`.
#'
#' @return A numeric array of shape \eqn{n_\mathrm{samples} \times R^{2
#'   \mathrm{dim}}} storing the sampled points.
#'
#' @export
#' @examplesIf reticulate::py_module_available("gudhi")
#' torus(10, 1)
torus <- function(n_samples, dim, sample = c("random", "grid")) {
  n_samples <- as.integer(n_samples)
  dim <- as.integer(dim)
  sample <- rlang::arg_match(sample)
  if (sample == "random" && n_samples > 150 && dim <= 5)
    gd$datasets$generators$points$torus(
      n_samples = n_samples,
      dim = dim,
      sample = sample
    )
  else
    gd$datasets$generators$points$ctorus(
      n_samples = n_samples,
      dim = dim,
      sample = sample
    )
}
