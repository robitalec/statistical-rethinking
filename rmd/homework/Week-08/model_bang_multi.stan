data {
	// Integers for number of rows, and number of districts
  int<lower=0> N;
  int<lower=0> N_district;

  // District and contraception, expecting integers of length N
	int district[N];
	int contraception[N];
}
parameters {
	// Alpha vector matching length of number of districts
	vector[N_district] alpha;
	real<lower=0> sigma;

	// Hyper parameter alpha bar
	real alpha_bar;
}
model {
  // Hyper priors: alpha bar and sigma
	alpha_bar ~ normal(0, 1.5);
	sigma ~ exponential(1);

	// Priors
  // Alpha is distributed normally
  alpha ~ normal(alpha_bar, sigma);

	// p vector matching length of number of districts
  vector[N_district] p;

	// For each district, alpha for district
  p = inv_logit(alpha[district]);

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
