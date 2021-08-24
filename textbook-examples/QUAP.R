# Quadratic approximation

library(rethinking)

data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18,]


precis(d2)

# alist = function arguments, not evaluated
flist <- alist(
	height ~ dnorm(mu, sigma),
	mu ~ dnorm(178, 20),
	sigma ~ dunif(0, 50)
)

quap(flist, data = d2)
