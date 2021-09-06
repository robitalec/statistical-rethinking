parameters{
  real alpha;
  real beta_judge;
  real beta_wine;
  real<lower=0> sigma;
}
model{
  sigma ~ exponential(1);
  beta_wine ~ normal(0, 0.5);
  beta_judge~normal(0, 0.5);
  alpha~normal(0, 0.2);
}
