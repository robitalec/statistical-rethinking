data {
  int<lower=0> N;
  int<lower=0> N_judges;
  int<lower=0> N_wines;
}
parameters{
  real alpha;
  vector[N_judges] beta_judge;
  vector[N_wines] beta_wine;
  real<lower=0> sigma;
}
model{
  sigma ~ exponential(1);
  beta_wine ~ normal(0, 0.5);
  beta_judge~normal(0, 0.5);
  alpha~normal(0, 0.2);
}
