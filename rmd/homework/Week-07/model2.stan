data {
  int N;
  int K;
  int N_edu;
  int response[N];
  int action[N];
  int intention[N];
  int contact[N];
  int education[N];
  vector[N_edu - 1] alpha;
}
parameters {
	// Cut points are the positions of responses along cumulative odds
  ordered[K] cutpoints;
  real beta_action;
  real beta_intention;
  real beta_contact;
  real beta_education;

  // Vector N reals that sum to 1
  simplex[7] delta;
}
model {
  vector[N] phi;
  vector[N_edu] delta_j;

  delta ~ dirichlet( alpha );
  delta_j = append_row(0, delta);

	for (i in 1:N) {
		phi[i] = beta_action * action[i] + beta_contact * contact[i] + beta_intention * intention[i];
		response[i] ~ ordered_logistic(phi[i], cutpoints);
	}

  cutpoints ~ normal(0, 1.5);
  beta_action ~ normal(0, 1);
  beta_contact ~ normal(0, 1);
  beta_intention ~ normal(0, 1);
  beta_education ~ normal(0, 1);
}
