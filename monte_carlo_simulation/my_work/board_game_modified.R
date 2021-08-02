## Modify the simulation of the board game example in Section 2.5
## to incorporate a random starting point, which we take to be
## squares 0 to 7 with probability 1/8 each.
boardsim <- function(nreps) {
  count4 <- 0
  countbonusgiven4 <- 0
  for (i in 1:nreps) {
    start_position <- sample(x=c(0:7), size=1)
    position <- start_position + sample(1:6, 1)
    if (position == 3) {
      bonus <- TRUE
      position <- (position + sample(1:6, 1)) %% 8
    } else bonus <- FALSE
    
    if (position == 4) {
      count4 <- count4 + 1
      if (bonus) countbonusgiven4 <- countbonusgiven4 + 1
    }
  }
  return(countbonusgiven4/count4)
}

# boardsim(1000)

## Add code to find P(X=7), where X is the position after one turn (including bonus, if any).
board_pos_prob <- function(pos, nreps) {
  count_all <- 0
  count_pos <- 0
  for (i in 1:nreps) {
    count_all <- count_all + 1
    position <- sample(1:6, 1)
    if (position == 3) {
      position <- (position + sample(1:6, 1)) %% 8
    }
    if (position == pos) {
      count_pos <- count_pos + 1
    }
  }
  return(count_pos / count_all)
}

board_pos_prob(7, 1000)