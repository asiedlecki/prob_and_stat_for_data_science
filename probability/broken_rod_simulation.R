## Say a glass rod drops and breaks into 5 random pieces. 
## Let's find the probability that the smallest piece has length below 0.02.

minpiece <- function(k) {
  breakpts <- sort(runif(k-1))
  lengths <- diff(c(0, breakpts, 1))
  min(lengths)
}

# returns the approximate probability
# that the smallest of k pieces will
# have length less than q

bkrod <- function(nreps, k, q) {
  minpieces <- replicate(nreps, minpiece(k))
  mean(minpieces < q)
}

bkrod(10**4, 5, 0.02)