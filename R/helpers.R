

#' Retrieve a variable from the parent environment
#' rather than the current environment.
#' @param x the variable to retrieve from the parent
#'          environment of the calling frame.
#' @export
.. <- function(x) {
  name <- deparse(substitute(x))
  o <- get(x=name, envir=parent.env(parent.frame()))
  return(o)
}

