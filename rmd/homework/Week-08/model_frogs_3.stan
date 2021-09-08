data {
	int N;
	int survival[N];
	int density[N];
	int tank[N];
	int size[N];
}
parameters {
	real sigma;
	real alpha[N];
	real alpha_bar;
	real beta_size;
}
transformed parameters {
	vector[N] p;

	for (i in 1:N) {
		p[i] = inv_logit(alpha[i] + beta_size * size[i]);
	}
}
model {
	alpha ~ normal(alpha_bar, sigma);
	beta_size ~ normal(0, 0.5);
	sigma ~ exponential(1);
	for (i in 1:N) {
		survival[i] ~ binomial(density[i], p[i]);
	}
}
