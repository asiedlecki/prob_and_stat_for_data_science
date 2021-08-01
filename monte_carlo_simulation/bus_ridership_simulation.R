## Simulation of tossing a coin:

toss_coin <- function() {
  if (runif(1) < 0.5)
    heads <- TRUE else heads <- FALSE
    return(heads)
}


## runif() can be used to simply simulate whether a simple event occured or not.
## This function generates random numbers from the interval (0,1), with all the points
## inside being equally likely. For instance, probability that the functions returns
## a value in (0, 0.5) is 0.5.

## Bus ridership:
## Finding probability that after visiting the tenth stop, the bus is empty.
## This is too complicated to solve analytically, but can be easily simulated.

nreps <- 10000
nstops <- 10
count <- 0
for (i in 1:nreps) {
  passengers <- 0
  for (j in 1:nstops) {
    if (passengers > 0) # any alight?
      for (k in 1:passengers)
        if (runif(1) < 0.2)
          passengers <- passengers - 1
    newpass <- sample(0:2, 1, prob=c(0.5, 0.4, 0.1))
    passengers <- passengers + newpass
  }
  if (passengers == 0) count <- count + 1
}
print(count/nreps)