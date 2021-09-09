data {
  int<lower=0> N;
  int<lower=0> N_district;
	int district[N];
	int contraception[N];
}
parameters {
	vector[N_district] alpha;
}
model {
  vector[N] p;
	alpha ~ normal(0, 1.5);
  for (i in 1:N) {
  	p[i] = inv_logit(alpha[district[i]]);
  }
  contraception ~ bernoulli(p);
}
