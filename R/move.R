
#' Function for moving a variable from one environment to another
#' by name.
#' @param x name of variable to be moved.
#' @param from source environment
#' @param to target environment
#' @param clobber logical environment inidcating whether to allow
#'        overwriting of variables in the target environment.
#' @export
move <- function(x, from, to, clobber=FALSE) {
  if (!clobber && (x %in% ls(to))) {
    msg <- paste("Variable'", x, "'already present in environment ", 
                 deparse(substitute(to)))
    stop(msg)
  }
  value <- get(x, envir=from, inherits=FALSE)
  assign(x=x, value=value, envir=to)
}

#' Move all objects from one environment to another
#' @param from environment to pull objects from.
#' @param to environment to write objects to.
move_all <- function(from, to) {
  object_names <- ls(name=from, all.names=TRUE)
  for (name in object_names) {
    o <- get(x=name, envir=from)
    move(x=name, from=from, to=to)
  }
  return(TRUE)
}

#' Move function objects from one environment to another
#' @param from environment to check for function objects. This
#'        environment must be _only_ contain functions.
#' @param to environment to write funciton objects to.
move_all_functions <- function(from, to) {
  function_names <- ls(name=from, all.names=TRUE)
  for (name in function_names) {
    f <- get(x=name, envir=from)
    if (is.function(f))
      move(x=name, from=from, to=to)
    else {
      msg <- paste0("Only functions can be passed in 'functions'",
                   " block.  Data should use data block.")
      stop(msg)
    }
  }
  return(TRUE)
}

#' Move all objects from one environment to another 
#' but refuse to overwrite.
move_all_noclobber <- function(from, to) {
  object_names <- ls(name=from, all.names=TRUE)
  target_names <- ls(name=to, all.names=TRUE)
  for (name in object_names) {
    if (name %in% target_names) {
      msg <- paste("Name '", name, "' already present in",
                   " target environment.", sep='')
      stop(msg)
    } else { 
      move(x=name, from=from, to=to)
    }
  }
  return(TRUE)
}




