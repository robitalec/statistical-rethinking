data {
  int N;
  int K;
  int response[N];
  int action[N];
  int intention[N];
  int contact[N];
}
parameters {
	// Cut points are the positions of responses along cumulative odds
  ordered[K] cutpoints;
  real beta_action;
  real beta_intention;
  real beta_contact;
}
model {
  vector[N] phi;

	for (i in 1:N) {
		phi[i] = beta_action * action[i] + beta_contact * contact[i] + beta_intention * intention[i];
		response[i] ~ ordered_logistic(phi[i], cutpoints);
	}

  cutpoints ~ normal(0, 1.5);
  beta_action ~ normal(0, 0.5);
  beta_contact ~ normal(0, 0.5);
  beta_intention ~ normal(0, 0.5);
}
generated quantities {
  vector[N] phi;
	vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = ordered_logistic_lpmf(response[i] | phi[i], cutpoints);
  }
}
