\name{simachine-package}
\alias{simachine-package}
\alias{simachine}
\docType{package}
\title{
\packageTitle{simachine}
}
\description{
\packageDescription{simachine}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{simachine}
\packageIndices{simachine}
The package is used to run simulation using immutable input
data, a code snippet ('quote({...})') for transforming the
data, inital state used to define simulation parameters, 
a simulation code snippet run once per iteration of the simulation
to describe how state evolves, and a generated quantities 
code snippet that does not evolve state but does emit derived
quantities (perhaps random).  This is modelled on the 'Stan'
program structure.  Data, transformed data, and simulation 
state are automatically stored in appropriate containers.
}
\author{
\packageAuthor{simachine}

Maintainer: \packageMaintainer{simachine}
}
\keyword{ package }
\examples{
data <- new.env()
data$n_steps <- 500
data$n_locations <- 4
data$mu <- 3
data$beta <- matrix(data=c(
  rnorm(n=data$n_locations, mean=.8, sd=.05),
  -rnorm(n=data$n_locations, mean=.2, sd=.05)
), ncol=2)
data$gamma <- .2
data$sigma <- .1
data$.. <- ..

data_transformations <- quote({})

initial_states <- new.env()
initial_states$theta <- rep(data$mu, data$n_locations)
initial_states$Lambda <- exp(initial_states$theta)
names(initial_states$theta) <- letters[1:data$n_locations]

simulation <- quote({
  theta <- mu + beta[,1]*theta + rnorm(n=n_locations, mean=0, sd=sigma)
  if (step > 2)
    theta <- mu + beta[,1]*..(theta)[,step-1] + beta[,2]*..(theta)[,step-2] +
      rnorm(n=n_locations, mean=0, sd=sigma)
  Lambda <- exp(theta)
})

generated_quantities <- quote({
  x <- rpois(n=length(Lambda), lambda=Lambda)
  y <- matrix(data=0, nrow=length(Lambda), ncol=n_steps)
  if (step > 1) {
    for (i in step:1) {
      delay <- step - i
      p <- pgamma(delay,2,gamma)
      y[,i] <- rbinom(n=length(x), size=..(x)[,i], prob=p)
    }
    rm(i); rm(p); rm(delay)
  }
})

o <- run_one(data=data, data_transformations=data_transformations,
         initial_states=initial_states, simulation=simulation,
         generated_quantities=generated_quantities,
         n_steps=data$n_steps)


}
