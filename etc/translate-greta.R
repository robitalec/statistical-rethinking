library(rethinking)
library(greta)


data(Howell1)

d <- Howell1[Howell1$age >= 18,]

m <- quap(
	alist(
		height ~ dnorm(mu, sigma),
		mu <- a + b * (weight - mean(d$weight)),
		a ~ dnorm(178, 20),
		b ~ dnorm(0, 1),
		sigma ~ dunif(0, 50)
	),
	data = d
)

# Greta
library(greta)

# data
weight <- as_data(scale(d$weight)) #x
height <- as_data(d$height) #y

# variables and priors
int <- normal(178, 20)
coef <- normal(0, 1)
sigma <- uniform(0, 50)

# operations
mu <- int + coef * weight

# likelihood
distribution(height) <- normal(mu, sigma)

# defining the model
gm <- model(int, coef, sigma)
draws <- mcmc(gm)
library (bayesplot)
mcmc_trace(draws, facet_args = list(nrow = 3, ncol = 1))
mcmc_intervals(draws)
