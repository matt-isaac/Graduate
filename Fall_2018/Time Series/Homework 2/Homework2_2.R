A <- matrix(data = c(1, -0.2, 0.48, -0.2, 1.48, 0, 0.48, -0.2, 1 ), nrow = 3, byrow = TRUE)
B <- matrix(data = c(1, 0, 0), nrow = 3, byrow = TRUE)

g <- solve(a = A, b = B)
g <- as.vector(g)

gammas <- c(g, rep(0, 28))

for(k in seq(4,31)){
  k_1 <- gammas[k-1]
  k_2 <- gammas[k-2]
  gammas[k] <- (-0.2 * k_1) + (-0.48 * k_2)
}

rho <- gammas/gammas[1]

plot(rho)
abline(a = 0, b = 0)
