## Find the approximate value of expected number of passengers on the bus as it leaves the tenth stop.
## Bus ridership problem is described in probability/bus_ridership_simulation.R.
## Code below is modified version of mentioned files's code.

nreps <- 10000
nstops <- 10
total <- 0
for (i in 1:nreps) {
  passengers <- 0
  for (j in 1:nstops) {
    if (passengers > 0)
      for (k in 1:passengers)
        if (runif(1) < 0.2)
          passengers <- passengers - 1
    newpass <- sample(x=0:2, size=1, prob=c(0.5, 0.4, 0.1))
    passengers <- passengers + newpass
  }
  total <- total + passengers
}
print(total/nreps)