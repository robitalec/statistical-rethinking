data {
  int<lower=0> N;
  int<lower=0> N_gender;
  int<lower=0> N_department;
  int awards[N];
  int applications [N];
  int index_gender[N];
  int index_department[N];
}
parameters{
  vector[N_gender] alpha;
  vector[N_department] beta;
}
model{
  vector[N] p;
  alpha ~ normal(-1, 1);
  beta ~ normal(0, 0.25);

  p = inv_logit(alpha[index_gender] + beta[index_department]);
  awards ~ binomial(applications, p);
}
