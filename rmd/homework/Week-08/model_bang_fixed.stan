data {
  int<lower=0> N;
  int<lower=0> N_district;
	int district[N];
	vector[N] contraception;
}
parameters {
	real sigma;
	real alpha;
	vector[N_district] beta_district;
}
model {
	alpha ~ normal(0, 0.5);
	sigma ~ exponential(1);
	beta_district ~ normal(0, 0.5);

  vector[N] mu;
  mu = beta_district[district];
  contraception ~ normal(mu, sigma);
}
