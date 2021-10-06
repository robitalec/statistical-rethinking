data {
  int<lower=0> N;
  int<lower=0> N_gender;
  int<lower=0> N_discipline;
  int awards[N];
  int applications [N];
  int index_gender[N];
  int index_discipline[N];
}
parameters{
  vector[N_gender] alpha;
  vector[N_discipline] beta;
}
model{
  vector[N] p;
  alpha ~ normal(-1, 1);
  beta ~ normal(0, 0.25);

  p = inv_logit(alpha[index_gender] + beta[index_discipline]);
  awards ~ binomial(applications, p);
}
