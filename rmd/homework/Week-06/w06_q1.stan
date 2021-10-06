data {
  int<lower=0> N;
  int<lower=0> N_gender;
  int awards[N];
  int applications [N];
  int index_gender[N];
}
parameters{
  vector[N_gender] alpha;
}
model{
  vector[N] p;
  alpha ~ normal(-1, 1);

  p = inv_logit(alpha[index_gender]);
  awards ~ binomial(applications, p);
}
