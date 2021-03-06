
#' Function for storing repeated values of a variable from 
#' one environment in another by name.  
#' @param x name of variable to be stored.
#' @param from source environment
#' @param to target environment
#' @param step simulation iteration
#' @param n_steps total number of simulation steps
#' @export
store <- function(x, from, to, step, n_steps) {
  value <- get(x, envir=from, inherits=FALSE)
  if (!exists(x, to, inherits=FALSE)) {
    if (isTRUE(is.vector(value))) {
      value <- matrix(data=c(value, rep(0,length(value)*(n_steps-1))), 
                      nrow=length(value), ncol=n_steps)
      assign(x=x, value=value, envir=to)
    } else if (isTRUE(is.array(value))) {
      value <- array(data=c(value, rep(0,length(value)*(n_steps-1))),
                     dim=c(dim(value), n_steps))
      assign(x=x, value=value, envir=to)
    } else if (isTRUE(is.data.frame(value))) {
      blank_value <- value
      for (c in colnames(blank_value)) {
        c_class <- class(blank_value[[c]])
        if (c_class != 'factor')
          blank_value[[c]] <- as(NA,c_class)
        else 
          blank_value[[c]] <- factor(rep(NA,nrow(blank_value)))
      }
      value[['step']] <- 1
      for (i in 2:n_steps) {
        blank_value[['step']] <- i
        value <- rbind(value, blank_value)
      }
    } else 
      stop("Only vectors, matrices, arrays, and data frames are currently handled by 'store'.")
  } else {
    assign(x='value', value=value, envir=to)
    if (isTRUE(is.vector(value))) {
      text <- paste0(x,'[,step] <- value')
      eval(parse(text=text), envir=to)
    } else if (isTRUE(is.array(value))) {
      eval(parse(text=paste0(x,'[', paste0(rep(',',length(dim(value))), collapse=''),'step] <- value')),
           envir=to)
    } else if (isTRUE(is.data.frame(value))) {
      start <- (step-1)*nrow(value)+1
      end <-   start +  nrow(value)-1
      eval(parse(text=paste0(x,'[',start,':',stop,',] <- value')), envir=to)
    } else 
      stop("Only vectors, matrices, arrays, and data frames are currently handled by 'store'.")
    rm(value, envir=to)
  }
}


#' Function for storing repeated values of a variable from 
#' one environment.
#' @param from source environment
#' @param to target environment
#' @param step simulation iteration
#' @param n_steps total number of simulation steps
#' @export
store_all <- function(from, to, step, n_steps) {
  object_names <- ls(from)
  target_names <- ls(to)
  for (name in object_names) {
    if (name != 'step') 
      store(x=name, from=from, to=to, step=step, n_steps=n_steps)
  }
  return(TRUE)
}





