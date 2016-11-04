
#' Create a sandbox environment as instructed. 
#' @param sandbox either 1) TRUE in which case the returned environment is
#'        is an environment (sandbox) with a parent below the global environment at
#'        the time the call is made.  This is important because packages
#'        loaded after the sandbox is created will not be visible to the
#'        sandbox! 2) FALSE in which case the sandbox has as a parent whatever
#'        environment is the parent.frame of the environment where this
#'        function was called; or 3) an environment in which case the
#'        returned sandbox has the environment as a parent.
#' @return a new environment with the specified parent.
#' @export
create_sandbox <- function(box=TRUE) {
  if (isTRUE(box)) {
    isolation_env <- parent.env(.GlobalEnv)
  } else {
    if (is.environment(box))
      isolation_env <- box
    else {
      isolation_env <- parent.frame()
    }
  }
  sandbox_env <- new.env(parent=isolation_env)
  return(sandbox_env)
}


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

