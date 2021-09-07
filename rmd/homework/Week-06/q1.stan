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
  real<lower=0> sigma;
  real<lower=0,upper=1> theta;
}
model{
  alpha ~ normal(0, 0.2);
  beta_gender ~ normal(0, 0.25);
  beta_discipline ~ normal(0, 0.25);
  sigma ~ exponential(1);
  theta ~ beta(1, 1);
  awards ~ binomial(N, theta);
}
