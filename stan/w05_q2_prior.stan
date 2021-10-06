data {
  int<lower=0> N;
  int<lower=0> N_flights;
  int<lower=0> N_wine_american;
  int<lower=0> N_judge_american;
}
parameters{
  real alpha;
  vector[N_flights] beta_flights;
  vector[N_wine_american] beta_wine_american;
  vector[N_judge_american] beta_judge_american;
  real<lower=0> sigma;
}
model{
  alpha ~ normal(0, 0.2);
  beta_flights ~ normal(0, 0.5);
  beta_wine_american ~ normal(0, 0.5);
  beta_judge_american ~ normal(0, 0.5);
  sigma ~ exponential(1);
}
