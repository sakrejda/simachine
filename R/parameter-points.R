
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
  return(points)
}

#' Expand a fixed grid of points in parameter space.
#' @param arg_bounds, n_arg_points.
#' @return data frame of points uniformly distributed in parameter
#'         space.
#' @export grid_parameter_points
fixed_parameter_points <- function(arg_values) {
  points <- do.call(what=expand.grid, args=arg_values) %>%
    apply(1, as.list)
  return(points)
}

#' For a list check whether a given entry is a leaf (in the parameter
#' definition language).
is_leaf <- function(x) {
  has_min_max <- isTRUE(all(c('min','max') %in% names(x)))
  has_no_lists <- isTRUE(!any(sapply(x, is.list)))
  if (has_min_max && has_no_lists)
    return(TRUE)
  else
    return(FALSE)
}


#' Tree (list) apply
#' @param x the tree-shaped list
#' @param leaf_def function which takes a node and returns TRUE only if
#'        the node is a leaf.
#' @param action_def function to run on each leaf node.
#' @return a list matching the tree structure and each node processed by
#'         the provided function.
#' @export tree_apply
tree_apply <- function(x, leaf_def, action_def) {
  if (leaf_def(x)) {
    o <- action_def(x)
  } else {
    o <- lapply(x, tree_apply, leaf_def=leaf_def, action_def=action_def)
  }
  return(o)
}

#' Create a grid of points in parameter space with uniformly drawn
#' random marginals for each parameter within parameter-specific bounds.
#' @param x, n_points.
#' @return data frame of points uniformly distributed in parameter
#'         space.
#' @export random_parameter_trees
random_parameter_trees <- function(x, n_points) {
  o <- list()
  f <- is_leaf
  g <- function(x) runif(n=1, min=x[['min']], max=x[['max']])
  for ( i in 1:n_points ) {
    o[[i]] <- tree_apply(x, f, g)
  }
  return(o)
}



