

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
  g <- function(x) if (!leaf_def(x)) {
      tree_apply(x, leaf_def, action_def) 
    } else {
      action_def(x)
    }
  o <- lapply(x, g) 
  return(o)
}

#' Delete NULL parameters from a parameter list (tree).
#' @parameters parameter (tree) to search.
#' @return parameter (tree) with NULL values removed.
tree_null_delete <- function(parameters) {
  not_null <- function(x) !(is.null(x) | all(sapply(x, is.null)))
  parameters <- Filter(not_null, parameters)
  o <- lapply(parameters, function(x) if (is.list(x)) 
    tree_null_delete(x) else x)
  return(o)
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

#' Overwrite unused parameters with NULL.
#' @parameters a list of named parameters.
#' @used a character vectors of parameters to keep.
#' @return a list of named parameters.
delete_unused_parameters <- function(parameters, used) {
  o <- list()
  f <- function(x) !any(sapply(x, is.list))
  g <- function(x) {
    o <- x[names(x) %in% used]
    if (length(o) == 0)
      return(NULL)
    else 
      return(o)
  }
  for ( i in 1:length(parameters) ) {
    o[[i]] <- tree_apply(parameters, f, g)
  }
  o <- tree_null_delete(o)
  return(o)
}

#' 
thin_parameter_tree <- function(parameters) {
  for (i in 1:length(parameters))
    for (j in 1:length(parameters))
      if ( (i != j) && identical(parameters[[i]], parameters[[j]]))
        parameters[[j]] <- NULL
  return(parameters)
}


