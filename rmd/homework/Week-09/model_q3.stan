data {
	// Integers for number of rows, and number of districts
  int<lower=0> N;
  int<lower=0> N_district;

  // K categories
  int K;
  vector[K] alpha_k;

  // District, contraception and urban, expecting integers of length N
	int district[N];
	int contraception[N];
	int urban[N];

	// Also scale_age, n_children
	real scale_age[N];
	int n_children[N];
}
parameters {
	// Alpha and beta urban vectors matching length of number of districts
	vector[N_district] alpha;

	// Beta urban vector matching length of number of districts
	vector[N_district] beta;

	// Hyper parameter alpha bar, beta bar
	real alpha_bar;
	real beta_bar;

	// scale_age and n_children
	real beta_scale_age;
	real beta_children;

	// Correlation matrix, sigma
	// 2 represents the number of predictors
	corr_matrix[2] Rho;
	vector<lower=0>[2] sigma;

	simplex[3] delta;
}
model {
	// p vector matching length of number of districts
  vector[N] p;

  // Hyper priors: alpha bar, beta urban bar, sigma and Rho
	alpha_bar ~ normal(0, 1);
	beta_bar ~ normal(0, 0.5);
	sigma ~ exponential(1);
	Rho ~ lkj_corr(2);

	//
	vector[K] delta_shell;
	delta ~ dirichlet(alpha_k);
	delta_shell = append_row(0, delta);

	// Multivariate normal
  {
	  vector[2] YY[N_district];
	  vector[2] MU;
	  MU = [alpha_bar, beta_bar]';
	  for (j in 1:N_district) {
	  	YY[j] = [alpha[j], beta[j]]';
	  }
	  YY ~ multi_normal(MU, quad_form_diag(Rho, sigma));
  }

  // Beta scale_age prior
	beta_scale_age ~ normal(0, 1.5);
	beta_children ~ normal(0, 1.5);

	// For each for in data, alpha and beta for that row's district
  for (i in 1:N) {
  	p[i] = inv_logit(alpha[district[i]] + beta[district[i]] * urban[i] + beta_scale_age * scale_age[i] + beta_children * sum(delta_shell[1:n_children[i]]) * n_children[i]);
  }

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
