data {
  int N;
  int K;
  int N_edu;
  int response[N];
  int action[N];
  int intention[N];
  int contact[N];
  int education[N];
  vector[N_edu - 1] alpha;
}
parameters {
	// Cut points are the positions of responses along cumulative odds
  ordered[K] cutpoints;
  real beta_action;
  real beta_intention;
  real beta_contact;
  real beta_education;

  // Vector N reals that sum to 1
  simplex[7] delta;
}
model {
  vector[N] phi;
  vector[N_edu] delta_j;

  delta ~ dirichlet(alpha);
  delta_j = append_row(0, delta);

	for (i in 1:N) {
    // add beta education  * sum delta j, up to current i's education
    phi[i] = beta_education * sum(delta_j[1:education[i]]) +
      beta_action * action[i] +
      beta_contact * contact[i] +
      beta_intention * intention[i];
    response[i] ~ ordered_logistic(phi[i], cutpoints);
	}

  cutpoints ~ normal(0, 1.5);
  beta_action ~ normal(0, 1);
  beta_contact ~ normal(0, 1);
  beta_intention ~ normal(0, 1);
  beta_education ~ normal(0, 1);
}

//////////////
data {
	// Integers for number of rows, and number of districts
  int<lower=0> N;
  int<lower=0> N_district;

  // District, contraception and urban, expecting integers of length N
	int district[N];
	int contraception[N];
	int urban[N];

	// Also scale_age and n_children
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
}
model {
	// p vector matching length of number of districts
  vector[N] p;

  // Hyper priors: alpha bar, beta urban bar, sigma and Rho
	alpha_bar ~ normal(0, 1);
	beta_bar ~ normal(0, 0.5);
	sigma ~ exponential(1);
	Rho ~ lkj_corr(2);

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
  	p[i] = inv_logit(alpha[district[i]] + beta[district[i]] * urban[i] + beta_scale_age * scale_age[i] + beta_children * n_children[i]);
  }

  // Contraception if distributed with bernoulli, p
  contraception ~ bernoulli(p);
}
