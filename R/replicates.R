
#' Function for running replicates of simulation.
#'
#' @param functions An environment (could be coerced from a list) with
#'        any additional functions needed by the simulation.  
#' @param data An environment (could be coerced from a list) with all
#'        immutable data for the simulation.  It may include code
#'        snippets (e.g.-quote({a <- 3; o <- f(x)}))
#' @param data_transformations code snippet run once, created by quoting a
#'        bracketed expression, (e.g.-quote({a <- 3; b <- 3*a})), which
#'        will be used to transform data once prior to simulation.
#' @param simulation function defining a single simulation, taking
#'        'parameters' as arguments.
#' @param simulation A code snippet, run once per replicate, created by 
#'        quoting a bracketed expression, (e.g.-quote({a <- 3; b <-
#'        -1*a}), which will be used to generate simulation output.  
#'        This code can not modify the data.
#' @param parameters list of lists.  Each element is a list of parameter
#'        values..
#' @param control control list for generating replicates, includes
#'        elements: 1) size, the number of replicates to generate per 
#'        parameter set; 2) output, a snippet run at the end of each
#'        simulation, should persist results. 
#'        used to summarize and persist simulation results.
#' @param sandbox if TRUE the parent.env where the call is made will not
#'        be available to the code.  For full effects see
#'        ?create_sandbox.
#' @export replicates
replicates <- function(
  functions = new.env(),
  data = new.env(),
  data_transformations = quote({}),
  simulation = quote({}), 
  generated_quantities = quote({}),
  parameters = list(),
  control = list(),
  sandbox=TRUE
) {
  M <- control[['size']]
  N <- length(parameters)
  output <- new.env()
  eval(data_transformations, data) 
  for ( m in 1:M ) {
    assign(x='step', value=m, envir=output)
    output_m <- new.env()
    for ( nn in 1:N ) {
      assign(x='step', value=nn, envir=output_m)
      sandbox_env <- create_sandbox(sandbox)
      move_all(from=data, to=sandbox_env)
      ## FIXME: move_all_functions should be in a try block.
      move_all_functions(from=functions, to=sandbox_env)
      simulation_env <- new.env(parent=sandbox_env)
      move_all(from=list2env(parameters[[nn]]), to=simulation_env)
      eval(simulation, simulation_env)
      eval(generated_quantities, simulation_env)
      for (name in control[['output']]) {
        store(x=name, from=simulation_env, to=output_m,
          step=nn, n_steps=N)
      }
      rm(sandbox_env)      
    }
    store_all(from=output_m, to=output, step=m, n_steps=M)
    rm(output_m)
  }
  return(output)
}







