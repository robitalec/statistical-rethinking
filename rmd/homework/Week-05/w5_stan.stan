data{
  vector[180] scale_score;
  int index_wine[180];
  int index_judge[180];
}
generated quantities {
	real sigma = exponential_rng(1);
  real beta_wine = normal_rng(0, 0.5);
  real beta_judge = normal_rng(0, 0.5);
  real alpha = normal_rng(0, 0.2);


	real mu = 0;
  mu += alpha + beta_judge * index_judge + beta_wine * index_wine;
}

parameters{
    real alpha;
    real beta_judge;
    real beta_wine;
    real<lower=0> sigma;
}
model{
    vector[180] mu;
    sigma ~ exponential( 1 );
    beta_wine ~ normal( 0 , 0.5 );
    beta_judge ~ normal( 0 , 0.5 );
    alpha ~ normal( 0 , 0.2 );
    for ( i in 1:180 ) {
        mu[i] = alpha + beta_judge * index_judge[i] + beta_wine * index_wine[i];
    }
    scale_score ~ normal( mu , sigma );
}

