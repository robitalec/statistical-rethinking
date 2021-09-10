data {
  int N;
  int K;
  int response[N];
  int action[N];
  int intention[N];
  int contact[N];
  int individual[N];
  int N_individual;
}
parameters {
	// Cut points are the positions of responses along cumulative odds
  ordered[K] cutpoints;
  real beta_action;
  real beta_intention;
  real beta_contact;
	real alpha_bar;
	real sigma;
}
transformed parameters {
  vector[N] phi;
  vector[N_individual] alpha;

	for (i in 1:N) {
		phi[i] = alpha[individual[i]] + beta_action * action[i] + beta_contact * contact[i] + beta_intention * intention[i];
	}
}
model {
	// Hyper parameter priors
	alpha_bar ~ normal(0, 1.5);
	sigma ~ exponential(1);

	// Priors
	alpha ~ normal(alpha_bar, sigma);

	response ~ ordered_logistic(phi, cutpoints);
  cutpoints ~ normal(0, 1.5);
  beta_action ~ normal(0, 0.5);
  beta_contact ~ normal(0, 0.5);
  beta_intention ~ normal(0, 0.5);
}
generated quantities {
	vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = ordered_logistic_lpmf(response[i] | phi[i], cutpoints);
  }
}