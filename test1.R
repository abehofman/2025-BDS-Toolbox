save_player <- TRUE
np <- 100
ni <- 10
N <- 10000
th_r <- rnorm(np, 0, 2)
be_r <- runif(ni, -2, 2)
th_e <- numeric(np)
be_e <- numeric(ni)
th_e_save <- numeric()
k <- .1
for (i in 1:N) { 
  ii <- sample(1:ni, 1)
  p <- sample(1:np, 1)
  s <- sample(0:1, 1, prob = c(1 - plogis(th_r[p] - be_r[ii]), plogis(th_r[p] - be_r[ii])))
  th_e[p] <- th_e[p] + k * (s - plogis(th_e[p] - be_e[ii]))
  be_e[ii] <- be_e[ii] - k * (s - plogis(th_e[p] - be_e[ii]))
  if (save_player) if (p == 1) th_e_save <- c(th_e_save, th_e[p])
}
plot(th_r, th_e)
plot(th_e_save)

lintr::lint("test1.R")

