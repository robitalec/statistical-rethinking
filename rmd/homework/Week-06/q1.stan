data {
  int<lower=0> N;
  int<lower=0> N_gender;
  int<lower=0> N_discipline;
  int awards;
  int index_gender;
  int index_discipline;
}
parameters{
  real alpha;
  vector[N_gender] beta_gender;
  vector[N_discipline] beta_discipline;
  // real<lower=0,upper=1> theta;
  real p[N];
}
model{
  alpha ~ normal(0, 0.2);
  beta_gender ~ normal(0, 0.25);
  beta_discipline ~ normal(0, 0.25);
  // theta ~ beta(1, 1);
//
//   real p;
  p ~ inv_logit(alpha + beta_gender[N_discipline]);
  awards ~ binomial(N, p);
}
