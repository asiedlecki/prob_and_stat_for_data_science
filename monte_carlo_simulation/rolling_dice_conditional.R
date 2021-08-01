## Supoose three fair dice are rolled. We wish to find the approximate
## probability that the first die shows fewer than 3 dots,
## given that the total number of dots for the 3 dice is more than 8

dicesim <- function(nreps) {
  count1 <- 0
  count2 <- 0
  for (i in 1:nreps) {
    d <- sample(1:6, 3, replace=T)
    # "among those lines in which A occurs"
    if (sum(d) > 8 ) {
      count1 <- count1 + 1
      if (d[1] < 3) count2 <- count2 + 1
    }
  }
  return(count2 / count1)
}