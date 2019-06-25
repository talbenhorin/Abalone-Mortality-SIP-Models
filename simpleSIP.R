# Ordinary differential equations describing the abalone WS-RLO system

rm(list=ls(all=TRUE)) #clears workspace

## Install these packages if you haven't already
# install.packages("Rtools")
# install.packages("deSolve")

## Load deSolve package
library(deSolve)

## Create a SIP function
sip <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <-  b*(S+I) - beta*S*P - (mu+c*(S+I))*S
    dI <-  beta*S*P - (mu+r+c*(S+I))*I
    dP <-  z*I - gamma*P
    
    return(list(c(dS, dI, dP)))
  })
}

### Set parameters
init       <- c(S = 5, I = 2, P = 0)
## b: reproduction; beta: transmission; mu: natural mortality; r: WS mortality; c: parasite shedding; gamma: parasite loss
parameters <- c(b = 0.32, beta = 0.03, mu = 0.15, r = 0.1, c = 0.025, z = 200, gamma = 52)
## Time frame
times      <- seq(0, 50, by = 0.0027)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sip, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

## Plot
plot(times,out$S+out$I)