#' is_leaf
#' 
#' For a list check whether a given entry is a leaf (in the parameter
#' definition language).
#' 
#' Parameter leaf nodes are defined by having a range (min and max). 
#' 
#' @param x potential leaf.
#' @return boolean
#' @export is_leaf
is_leaf <- function(x) {
  has_min_max <- isTRUE(all(c('min','max') %in% names(x)))
  has_no_lists <- isTRUE(!any(sapply(x, is.list)))
  if (has_min_max && has_no_lists)
    return(TRUE)
  else
    return(FALSE)
}

#' Create a grid of points in parameter space with uniformly drawn
#' random marginals for each parameter within parameter-specific bounds.
#' @param x, n_points.
#' @return data frame of points uniformly distributed in parameter
#'         space.
#' @export random_parameter_trees
random_parameter_trees <- function(arg_bounds, n_points) {
  o <- list()
  f <- is_leaf
  g <- function(x) runif(n=1, min=x[['min']], max=x[['max']])
  for ( i in 1:n_points ) {
    o[[i]] <- tree_apply(arg_bounds, f, g)
  }
  return(o)
}



