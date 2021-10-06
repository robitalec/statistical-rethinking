data {
  int<lower=0> N;
  int<lower=0> N_interactions;

  int index_interactions[N];

  vector[N] scale_score;
}
parameters{
  real alpha;
  vector[N_interactions] beta_interactions;
  real<lower=0> sigma;
}
model{
  alpha ~ normal(0, 0.2);
  beta_interactions ~ normal(0, 0.25);
  sigma ~ exponential(1);

  vector[N] mu;
  mu = alpha + beta_interactions[index_interactions];

  scale_score ~ normal(mu, sigma);
}
