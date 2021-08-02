## Say a glass rod drops and breaks into random number of pieces, taking on values 2, 3, 4
## with prbabilites 0.3, 0.3, 0.4.
## Let's find the probability that the smallest piece has length below 0.02.

minpiece <- function() {
  k <- sample(x=c(2, 3, 4), size=1, prob=c(0.3, 0.3, 0.4))
  breakpts <- sort(runif(k-1))
  lengths <- diff(c(0, breakpts, 1))
  min(lengths)
}

# returns the approximate probability
# that the smallest of k pieces will
# have length less than q

bkrod <- function(nreps, q) {
  minpieces <- replicate(nreps, minpiece())
  mean(minpieces < q)
}

bkrod(10**4, 0.02)