library(simachine)
context("environment handling")

test_that("function '..' works.", {
  sub_global_env <- parent.env(.GlobalEnv)
  parallel_global_env <- new.env(parent=sub_global_env)
  simulation_env <- new.env(parent=parallel_global_env)
  assign(x='..', value=.., envir=simulation_env)
  sim_values <- 1:20
  assign(x='values', value=sim_values, envir=simulation_env)
  sub_sim_values <- 20:1
  assign(x='values', value=sub_sim_values, envir=parallel_global_env)
  expect_true(eval(parse(text="is.vector(values)"), envir=simulation_env))
  expect_true(eval(parse(text="is.vector(values)"), envir=parallel_global_env))
  expect_equal(eval(parse(text="length(values)"), envir=simulation_env), length(sim_values))
  expect_equal(eval(parse(text="length(values)"), envir=parallel_global_env), length(sub_sim_values))
  expect_equal(eval(parse(text="values"), envir=simulation_env), sim_values)
  expect_equal(eval(parse(text="..(values)"), envir=simulation_env), sub_sim_values)
})



