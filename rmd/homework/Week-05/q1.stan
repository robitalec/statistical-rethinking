data {
  int<lower=0> N;
  int index_judge[N];
  int index_wine[N];
  vector[N] scale_score;
}
parameters{
  real alpha;
  vector[9] beta_judge;
  vector[20] beta_wine;
  real<lower=0> sigma;
}
model{
  alpha ~ normal(0, 0.2);
  beta_wine ~ normal(0, 0.5);
  beta_judge ~ normal(0, 0.5);
  sigma ~ exponential(1);

  vector[N] mu;
  mu = beta_wine[index_wine] + beta_judge[index_judge];
  scale_score ~ normal(mu, sigma);
}
