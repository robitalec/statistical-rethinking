data {
  int<lower=0> N;
  int<lower=0> N_gender;
  int<lower=0> N_discipline;
}
parameters{
  real alpha;
  vector[N_gender] beta_gender;
  vector[N_discipline] beta_discipline;
  real<lower=0> sigma;
}
model{
  alpha ~ normal(0, 0.2);
  beta_gender ~ binomial(N, 0.5);
  beta_discipline ~ normal(0, 0.25);
  sigma ~ exponential(1);
}
