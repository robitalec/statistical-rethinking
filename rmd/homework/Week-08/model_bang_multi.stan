data {
  int<lower=0> N;
  int<lower=0> N_district;
	int district[N];
	int contraception[N];
}
parameters {
	real sigma;
	real alpha_bar;
	vector[N_district] alpha;
	// vector[N_district] beta_district;
}
transformed parameters {
	vector[N] p;

	for (i in 1:N) {
		p[i] = inv_logit(alpha[i]);
	}
}
model {
	alpha_bar ~ normal(0, 0.25);
	alpha ~ normal(alpha_bar, sigma);
	sigma ~ exponential(1);
	// beta_district ~ normal(0, 0.5);

  vector[N] mu;
  mu = alpha[district];
  contraception ~ normal(mu, sigma);
}
