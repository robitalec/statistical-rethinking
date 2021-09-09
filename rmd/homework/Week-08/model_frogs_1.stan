data {
	int N;
	int survival[N];
	int density[N];
	int tank[N];
}
parameters {
	real sigma;
	real alpha[N];
	real alpha_bar;
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
	for (i in 1:N) {
		survival[i] ~ binomial(density[i], p[i]);
	}
}
