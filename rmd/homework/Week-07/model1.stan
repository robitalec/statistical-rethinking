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
  phi = beta_action * action + beta_contact * contact + beta_intention * intention;
  response ~ ordered_logistic(phi, cutpoints);
  cutpoints ~ normal(0, 1.5);
  bA ~ normal(0, 0.5);
  bI ~ normal(0, 0.5);
  bC ~ normal(0, 0.5);
}
