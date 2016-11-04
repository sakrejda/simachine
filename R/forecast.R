
#' Function for running a stepwise simulation.
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
#' @param sandbox if TRUE the parent.env where the call is made will not
#'        be available to the code.  For full effects see
#'        ?create_sandbox.
#' @export
forecast <- function(
  functions = new.env(),
  data = new.env(), 
  data_transformations = quote({}), 
  initial_states = new.env(), 
  simulation = quote({}),
  generated_quantities = quote({}), 
  n_steps = 1,
  sandbox = TRUE
) {
  sandbox_env <- create_sandbox(sandbox)
  ## FIXME: move_all_functions should be in a try block.
  move_all_functions(from=functions, to=sandbox_env)
  simulation_env <- new.env(parent=sandbox_env)
  move_all(from=data, to=sandbox_env)
  eval(data_transformations, sandbox_env) 
  ## FIXME: move_all_noclobber should be in a try block.
  move_all_noclobber(from=initial_states, to=simulation_env)
  for (step in 1:n_steps) {
    if ('step' %in% ls(sandbox_env)) {
      msg <- paste0("Variable 'step' already",
        " present in the execution environment. Please change this.")
      stop(msg)
    }
    assign(x='step', value=step, envir=sandbox_env)
    eval(simulation, simulation_env)
    eval(generated_quantities, simulation_env)
    store_all(from=simulation_env, to=sandbox_env, 
      step=step, n_steps=n_steps)
    rm(list='step', envir=sandbox_env)
  }
  return(sandbox_env)
}








