Z <- 2
exp(-Z)

t <- 0:20
N0 <- 100

Nt <- N0*exp(-Z*t)

plot(t, Nt, log="y")
exp(-Z)
