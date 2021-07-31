# roll d dice ; find P(total = k)

probtotk <- function(d, k, nreps) {
  count <- 0
  # do the experiment nreps times -- like doing nreps notebook lines
  for (rep in 1:nreps) {
    # roll d dice and find their sum
    total <- sum(sample(1:6, d, replace = TRUE))
    if (total == k) count <- count + 1
  }
  return(count/nreps)
}

# example
probtotk(3, 8, 10000)