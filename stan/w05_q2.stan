data {
  int<lower=0> N;
  int<lower=0> N_flights;
  int<lower=0> N_wine_american;
  int<lower=0> N_judge_american;

  int index_flight[N];
  int index_wine_american[N];
  int index_judge_american[N];

  vector[N] scale_score;
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

  vector[N] mu;
  mu = beta_flights[index_flight] + beta_wine_american[index_wine_american] + beta_judge_american[index_judge_american];
  scale_score ~ normal(mu, sigma);
}
