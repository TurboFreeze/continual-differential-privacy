### apple.r
### private Hadamard count mean sketch (HCMS)

# install.packages(c("pracma", "openssl")) # run to install dependencies
library(pracma)           # needed for Hadamard matrix
library(openssl)          # hashing
library(phangorn)         # fast Hadamard transform

source('private.r')


# client-side algorithm
hcms.client <- function (d, epsilon, hashes, m) {
  j <- sample(1:k, 1) # select hashing function
  v <- rep(0, m)
  v[hashes[[j]](d)] <- 1 # activate 1 bit
  w <- fhm(v) # Hadamard transformation
  l <- sample(1:m, 1)
  b <- randomized.response(epsilon)
  w.tilde <- b * w[l]
  c(w.tilde, j, l)
}

# server-side computing sketch matrix
hcms.server.sketch <- function (data, epsilon, k, m) {
  n <- nrow(data)
  c <- (exp(epsilon) + 1) / (exp(epsilon) - 1)
  x <- k * c * data$w
  
  # fill in elements of sketch matrix
  M <- matrix(0, nrow=k, ncol=m)
  for (i in 1:n) {
    M[data$j[i], data$l[i]] <- M[data$j[i], data$l[i]] + x[i]
  }
  
  # reverse Hadamard transformation
  M <- M %*% t(hadamard(2^log(m, 2)))
  M
}

# server-side algorithm
hcms.server <- function(d, epsilon, sketch, hashes, n) {
  m <- ncol(sketch)
  k <- nrow(sketch)
  sketch.sum <- sum(apply(matrix(1:k), MARGIN=1, function (l) {sketch[l, hashes[[l]](d)]}))
  f <- (m / (m - 1)) * ((1 / k) * sketch.sum - n / m)
  f
}

# core algorithm
hcms.master <- function(data, epsilon, k, m, dict, hashes) {
  n <- length(data)
  
  # get local responses
  # locals <- data.frame(w=numeric(), j=numeric(), l=numeric())
  # for (i in 1:length(data)) {
  #   locals <- rbind(data.frame(hcms.client(data[i], epsilon, hashes, m)))
  #   if (mod(i, 1000) == 1) {
  #     print(i)
  #   }
  # }
  locals <- matrix(0, nrow=n, ncol=3)
  for (i in 1:n) {
    if (mod(i, 1000) == 1) {
      print(i)
    }
    locals[i,] <- hcms.client(data[i], epsilon, hashes, m)
  }
  locals <- data.frame(locals)
  names(locals) <- c("w", "j", "l")
  
  # construct the sketch matrix
  M <- hcms.server.sketch(locals, epsilon, k, m)
  print("Constructing DP histogram")
  
  # construct DP histogram
  hist.dp <- data.frame(d=character(), c=numeric())
  for (i in 1:length(dict)) {
    if (mod(i, 1000) == 1) {
      print(i)
    }
    d <- dict[i]
    d.res <- t(c(d, as.numeric(hcms.server(d, epsilon, M, hashes, n))))
    hist.dp <- rbind(hist.dp, data.frame(d.res))
  }
  hist.dp
}
