data {
	// Integers for number of rows, and number of districts
  int<lower=0> N;
  int<lower=0> N_district;

  // District, contraception and urban, expecting integers of length N
	int district[N];
	int contraception[N];
	int urban[N];
}
parameters {
	// Alpha and beta urban vectors matching length of number of districts
	vector[N_district] alpha;

	// Beta urban vector matching length of number of districts
	vector[N_district] beta_urban;


	// Hyper parameter alpha bar, beta urban bar
	real alpha_bar;
	real beta_urban_bar;

	// Correlation matrix, sigma
	corr_matrix[2] Rho;
	vector<lower=0>[2] sigma;

}
model {
	// p vector matching length of number of districts
  vector[N] p;

	// Priors
  // Alpha is distributed normally
  alpha ~ normal(alpha_bar, sigma);

	// Multivariate normal
  {
  vector[2] YY[N_district];
  vector[2] MU;
  MU = [alpha_bar, beta_urban_bar]';
  for (j in 1:N_district) YY[j] = [alpha[j], beta_urban[j]]';
  YY ~ multi_normal(MU, quad_form_diag(Rho, sigma));
  }

  // Hyper priors: alpha bar, beta urban bar, sigma and Rho
	alpha_bar ~ normal(0, 1.5);
	beta_urban_bar ~ normal(0, 1.5);
	sigma ~ exponential(1);
	Rho ~ lkj_corr(2);

	// For each for in data, alpha and beta_urban for that row's district
  for (i in 1:N) {
  	p[i] = inv_logit(alpha[district[i]] + beta_urban[district[i]] * urban[i]);
  }

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
