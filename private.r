### private.r
### library of differentially private mechanisms and algorithms

source("lib.r")         # load library of helper functions

# randomized response for local DP
randomized.response <- function (epsilon) {
  p <- exp(epsilon) / (exp(epsilon) + 1)
  response <- as.numeric(runif(1) < p)
  response * 2 - 1 
}
