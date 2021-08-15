### If we roll three dice, what is the probability that their total is 8?
# roll d dice ; find P(total = k)

# simulate roll of nd dice; 
# the possible return values are 1,2,3,4,5,6, all equally likely
roll <- function(nd) return(sample(1:6, nd, replace=TRUE))

probtotk <- function(d, k, nreps) {
  sums <- vector(length = nreps)
  # do the experiment nreps times
  sums <- replicate(nreps, sum(roll(d)))
  return(mean(sums == k))
}

# example
probtotk(3, 8, 10000)