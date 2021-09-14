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

	// Beta urban vector matching length of number of districts
	vector[N_district] beta_urban;

	real<lower=0> sigma;

	// Hyper parameter alpha bar, beta urban bar
	real alpha_bar;
	real beta_urban_bar;

	// Correlation matrix
	corr_matrix[2] Rho;
}
model {
	// p vector matching length of number of districts
  vector[N] p;

	// For each for in data, alpha and beta_urban for that row's district
  for (i in 1:N) {
  	p[i] = inv_logit(alpha[district[i]] + beta_urban[district[i]] * urban[i]);
  }

  // Hyper priors: alpha bar, beta urban bar and sigma
	alpha_bar ~ normal(0, 1.5);
	beta_urban_bar ~ normal(0, 1.5);
	sigma ~ exponential(1);

	// Priors
  // Alpha is distributed normally
  alpha ~ normal(alpha_bar, sigma);

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
