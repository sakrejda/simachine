
#' Create a grid of points in parameter space with uniformly drawn
#' random marginals for each parameter within parameter-specific bounds.
#' @param arg_bounds, n_arg_points.
#' @return data frame of points uniformly distributed in parameter
#'         space.
#' @export random_parameter_points
random_parameter_points <- function(arg_bounds, n_points) {
  n_points_per_dim <- n_points^(1/length(arg_bounds)) %>% ceiling
  o <- list()
  for (name in names(arg_bounds)) {
    o[[name]] <- runif(n=n_points_per_dim, min=arg_bounds[[name]][1], 
      max=arg_bounds[[name]][[2]])
  }
  points <- do.call(what=expand.grid, args=o)
  return(points)
}


#' Create a grid of points in parameter space within parameter-specific
#' bounds.
#' @param arg_bounds, n_arg_points.
#' @return data frame of points uniformly distributed in parameter
#'         space.
#' @export grid_parameter_points
grid_parameter_points <- function(arg_bounds, n_points) {
  n_points_per_dim <- n_points^(1/length(arg_bounds)) %>% ceiling
  o <- list()
  for (name in names(arg_bounds)) {
    o[[name]] <- seq(from=arg_bounds[[name]][1], to=arg_bounds[[name]][1],
      length.out=n_points_per_dim)
  }
  points <- do.call(what=expand.grid, args=o)
  return(points)
}




