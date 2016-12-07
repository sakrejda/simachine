#' Localize names
#'
#' Takes names of a list and inserts them as list elements.
#'
#' @param list
#' @return list
#' @export localize_names
localize_names <- function(x) {
  for (i in 1:length(x)) {
    if (is.list(x[[i]]))
      x[[i]][['name']] <- names(x)[[i]]
  }
  return(x)
}


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
  points <- do.call(what=expand.grid, args=o) %>% 
    apply(1, as.list)
  for(i in 1:length(points))
    points[[i]][['point_digest']] <- digest(points[[i]])
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
  points <- do.call(what=expand.grid, args=o) %>%
    apply(1, as.list)
  for(i in 1:length(points))
    points[[i]][['point_digest']] <- digest(points[[i]])
  return(points)
}

#' Expand a fixed grid of points in parameter space.
#' @param arg_bounds, n_arg_points.
#' @return data frame of points uniformly distributed in parameter
#'         space.
#' @export fixed_parameter_points
fixed_parameter_points <- function(arg_values) {
  points <- do.call(what=expand.grid, args=arg_values) %>%
    apply(1, as.list)
  for(i in 1:length(points))
    points[[i]][['point_digest']] <- digest(points[[i]])
  return(points)
}


