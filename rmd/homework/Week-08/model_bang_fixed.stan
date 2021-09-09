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
}
model {
	// p vector matching length of number of districts
  vector[N] p;

  // Alpha is distributed normally
	alpha ~ normal(0, 1.5);

	// For each for in data, alpha for that row's district
  // for (i in 1:N) {
  // 	p[i] = inv_logit(alpha[district[i]]);
  // }

  // Or vectorized
  p = inv_logit(alpha[district]);

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
