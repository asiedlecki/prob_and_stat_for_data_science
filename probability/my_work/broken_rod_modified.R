## Modify the simulation code in the broken-rod example, Section 2.6,
## so that the number of pieces will be random, taking on the values
## 2,3 and 4 with probabilities 0.3, 0.3 and 0.4

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