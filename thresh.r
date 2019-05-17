### thresh.r
### implementation of the Thresh algorithm for local DP Bernoulli means
### from https://arxiv.org/pdf/1802.07128.pdf

# global server function
thresh.server <- function (n, T, L, m, l, epsilon, delta) {
  # global estimate
  p.init <- -1
  # vote privacy counters
  vote.counters <- rep(0, n)
  # estimate privacy counters
  est.counters <- rep(0, n)
  # vote noise level
  innertemp <- 12 * m * T / delta
  vote.noise <- 4 * sqrt(2 * n * log(innertemp)) / (L - (3 / sqrt(2)) * sqrt(n * log(innertemp)))
  # estimate noise level
  innertemp2 <- 12 * T / delta
  estimate.noise <- sqrt(2 * log(innertemp2) / (2 * n)) / 
    (log(T, 10) * sqrt(log(innertemp2 * n) / (2 * l)) - sqrt(log(innertemp2) / (2 * n)))
  
  # extra storage variables
  votes <- rep(0, n)
  f <- rep(0, n)
  global.estimates <- rep(0, T + 1)
  global.estimates[1] <- p.init
  local.estimates <- rep(0, n)
  
  # threshold tau
  b.range <- -1:floor(log(T, 10))
  tau <- function(b) {
    2 * (b + 1) * sqrt(ln(12 * n * T / delta) / (2 * l))
  }
  
  # client voting
  thresh.vote <- function (i, t) {
    local.estimates[i] <- (1 / l)
    # find highest b such that difference greater than threshold
    b.star <- 0
    for (b in b.range) {
      if (abs(local.estimates[i] - local.estimates[f[t]]) > tau(b)) {
        b.star <- b
      }
    }
    vote.yes <- (vote.counters[i] < epsilon / 4) && (mod(t, 2^(floor(log(T, 10))-b.star)) == 0)
    ans <- 0
    if (vote.yes) {
      vote.counters[i] <- vote.counters[i] + vote.noise
      ans <- rbinom(1, 1, exp(vote.noise) / (exp(vote.noise) + 1))
    } else {
      ans <- rbinom(1, 1, 1 / (exp(vote.noise) + 1))
    }
    ans
  }
  
  # client estimation
  thresh.est <- function (i, t) {
    send.est <- est.counters[i] < epsilon / 4
    p.return <- NULL
    if (send.est) {
      est.counters[i] <- est.counters[i] + estimate.noise
      p.return <- rbinom(1, 1, (1 + local.estimates[i] * (exp(estimate.noise) - 1))
                         / exp(estimate.noise) + 1)
    } else {
      p.return <- rbinom(1 / (exp(estimate.noise) + 1))
    }
    p.return
  }
  
  
  # loop through epochs
  for (t in 1:T) {
    # each user votes
    for (i in 1:n) {
      votes[i] <- thresh.vote(i, t)
    }
    global.update <- mean(votes) > 1 / (exp(vote.noise) + 10) + sqrt(ln(10 * T / delta) / (2 * n))
    # check voting results
    if (global.update) {
      f[t] <- t
      # users publish
      published <- rep(0, n)
      for (i in 1:n) {
        published[i] <- thresh.est(i, t)
      }
      global.estimates[t + 1] <- mean((published * (exp(estimate.noise) + 1) - 1)/ (exp(estimate.noise) - 1))
    }
    else {
      # # publish noise
      # published <- rep(0, n)
      # for (i in 1:n) {
      #   published[i] <- rbin(n=1,prob=1 / (exp(estimate.noise) + 1))
      # }
      f[t] <- f[t - 1]
      global.estimates[i + 1] <- global.estimates[i]
    }
    
  }
}
