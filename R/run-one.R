
#' Function for running a structured simulation.
#' @param functions An enviornment (could be coerced from a list) with
#'        any additional functions needed by the simulation.  
#' @param data An environment (could be coerced from a list) with all
#'        immutable data for the simulation.
#' @param data_transformations code snippet run once, created by quoting a
#'        bracketed expression, (e.g.-quote({a <- 3; b <- 3*a})), which
#'        will be used to transform data once prior to simulation.
#' @param initial_states An environment (could be coerced from a list)
#'        with the initial state of simulation parameters.
#' @param simulation A code snippet, run once per step, created by 
#'        quoting a bracketed expression, (e.g.-quote({a <- 3; b <-
#'        -1*a}), which will be used to transform the simulation state
#'        once per step.  This code can not modify the data.
#' @param generated_quantities A code snippet, run once per step,
#'        after simulation, which will be used to generate output 
#'        dependent on the simulated state.
#' @param n_steps total number of simulation iterations to run.
#' @export
run_one <- function(
  functions = new.env(),
  data = new.env(), 
  data_transformations = quote({}), 
  initial_states = new.env(), 
  simulation = quote({}),
  generated_quantities = quote({}), 
  n_steps = 1
) {
  function_names <- ls(name=functions, all.names=TRUE)
  for (name in function_names) {
    f <- get(x=name, envir=functions)
    if (is.function(f))
      move(x=name, from=functions, to=parallel_global_env)
    else {
      msg <- paste0("Only functions can be passed in 'functions'",
                   " block.  Data should use data block.")
      stop(msg)
    }
  }
  sub_global_env <- parent.env(.GlobalEnv)
  parallel_global_env <- new.env(parent=sub_global_env)
  simulation_env <- new.env(parent=parallel_global_env)
  data_names <- ls(name=data, all.names=TRUE)
  for (name in data_names) 
    move(x=name, from=data, to=parallel_global_env)
  eval(data_transformations, parallel_global_env) 
  initial_state_names <- ls(name=initial_states)
  for (name in initial_state_names) {
    move(x=name, from=initial_states, to=simulation_env)
    if (name %in% data_names) {
      msg <- paste0("Names of simulation states can not mask names",
                    " of data objects.  Rename the object '", name, 
                    "' to continue.")
      stop(msg)
    }
  }
  for (step in 1:n_steps) {
    if ('step' %in% ls(parallel_global_env)) {
      msg <- paste0("Variable 'step' already",
        " present in the execution environment. Please change this.")
      stop(msg)
    }
    assign(x='step', value=step, envir=parallel_global_env)
    eval(simulation, simulation_env)
    eval(generated_quantities, simulation_env)
    for (name in ls(name=simulation_env)) {
      if (name %in% data_names) {
        msg <- paste0("Names of simulation states can not mask names",
                      " of data objects.  Rename the object '", name, 
                      "' to continue.")
        stop(msg)
      }
      if (name != 'step') 
        store(x=name, from=simulation_env, to=parallel_global_env, step=step, n_steps=n_steps)
    }
    rm(list='step', envir=parallel_global_env)
  }
  return(parallel_global_env)
}








