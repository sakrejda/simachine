#' Tree apply
#' 
#' Applies a function to tree leaves only.
#'
#' In this terminology a tree is a multi-level list, 
#' not necessarily balanced, and each tree branch ends
#' in a node which is defined _only_ by the `leaf_def` function.
#' At each leaf the `action_def` function is applied.
#' 
#' @param x the tree-shaped list
#' @param leaf_def function which takes a node and returns TRUE only if
#'        the node is a leaf.
#' @param action_def function to run on each leaf node.
#' @return a list matching the tree structure and each node processed by
#'         the provided function.
#' @export tree_apply
tree_apply <- function(tree, leaf_def, action_def) {
  g <- function(x) if (!leaf_def(x)) {
      tree_apply(x, leaf_def, action_def) 
    } else {
      action_def(x)
    }
  if (is.list(tree))
    tree <- lapply(tree, g) 
  return(tree)
}

#' Remove NULL leaves tree.
#' @param tree tree to trim.
#' @return parameter tree with NULL values removed.
tree_null_trim <- function(tree) {
  not_null <- function(x) !(is.null(x) | all(sapply(x, is.null)))
  tree <- Filter(not_null, tree)
  o <- lapply(tree, function(x) if (is.list(x)) 
    tree_null_delete(x) else x)
  return(o)
}


#' Tree delete branches.
#' @param tree to delete branches from.
#' @param names a character vectors of parameters to remove.
#' @return a list of named parameters.
tree_delete_branches <- function(tree, names) {
  stop("Set named branches to NULL.")
  tree <- tree_null_trim(tree)
  return(tree)
}

#' Delete trees which are identical from a forest (list of trees).
#' @param forest a list of trees.
#' @return a list of trees without duplicates.
thin_forest <- function(forest) {
  for (i in 1:length(forest))
    for (j in 1:length(forest))
      if ( (i != j) && identical(forest[[i]], forest[[j]]))
        forest[[j]] <- NULL
  return(forest)
}


