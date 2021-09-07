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
  real alpha;
  vector[N_gender] beta_gender;
  vector[N_discipline] beta_discipline;
}
model{
  alpha ~ normal(0, 0.2);
  beta_gender ~ normal(0, 0.25);
  beta_discipline ~ normal(0, 0.25);
  vector[N] p;
  p = inv_logit(alpha + beta_gender[index_gender] + beta_discipline[index_discipline]);
  awards ~ binomial(N, p);
}
