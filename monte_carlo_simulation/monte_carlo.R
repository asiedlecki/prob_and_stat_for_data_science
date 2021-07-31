# roll d dice ; find P(total = k)

probtotk <- function(d, k, nreps) {
  count <- 0
  # do the experiment nreps times -- like doing nreps notebook lines
  for (rep in 1:nreps) {
    sum <- 0
    # roll d dice and find their sum
    for (j in 1:d) sum <- sum + roll()
    if (sum == k) count <- count + 1
  }
  return(count/nreps)
}

# simulate roll of one die; the possible return
# values are 1,2,3,4,5,6, all equally likely
roll <- function() return(sample(1:6, 1))

# example
probtotk(3, 8, 10000)