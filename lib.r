### lib.r
### library of helper functions

# sign function
sgn <- function(x) {
  return(ifelse(x < 0, -1, 1))
}

# Laplace random draws with scale factor s
rlap <- function(mu=0, s=1, size=1) {
  p <- runif(size) - 0.5
  draws <- mu - s * sgn(p) * log(1 - 2 * abs(p))
  draws
}

# clipping function
clip <- function(x, lower, upper){
  x.clipped <- x
  x.clipped[x.clipped < lower] <- lower
  x.clipped[x.clipped > upper] <- upper
  x.clipped
}

# calculating RMSE
rmse <- function (actual, release) {
  sqrt(sum((actual - release) ^ 2))
}

