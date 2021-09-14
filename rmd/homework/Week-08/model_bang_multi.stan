data {
	// Integers for number of rows, and number of districts
  int<lower=0> N;
  int<lower=0> N_district;

  // District, contraception and urban, expecting integers of length N
	int district[N];
	int contraception[N];
	int urban[N];
}
parameters {
	// Alpha vector matching length of number of districts
	vector[N_district] alpha;

	// Beta urban
	real beta_urban;

	real<lower=0> sigma;

	// Hyper parameter alpha bar
	real alpha_bar;

}
model {
	// p vector matching length of number of districts
  vector[N] p;

	// For each for in data, alpha for that row's district
  for (i in 1:N) {
  	p[i] = inv_logit(alpha[district[i]] + beta_urban * urban[i]);
  }

  // Hyper priors: alpha bar and sigma
	alpha_bar ~ normal(0, 1.5);
	sigma ~ exponential(1);

	// Priors
  // Alpha is distributed normally
  alpha ~ normal(alpha_bar, sigma);

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
