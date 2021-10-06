library(rethinking)
data(bangladesh)
d <- bangladesh
dat_list <- list(
	C = d$use.contraception,
	did = as.integer( as.factor(d$district) ),
	urban = d$urban
)
m1.1 <- ulam(
	alist(
		C ~ bernoulli( p ),
		logit(p) <- a[did] + b[did]*urban,
		c(a,b)[did] ~ multi_normal( c(abar,bbar) , Rho , Sigma ),
		abar ~ normal(0,1),
		bbar ~ normal(0,0.5),
		Rho ~ lkj_corr(2),
		Sigma ~ exponential(1)
	) , data=dat_list , chains=4 , cores=4 )

"
data{
	int C[1934];
	int urban[1934];
	int did[1934];
}
parameters{
	vector[60] b;
	vector[60] a;
	real abar;
	real bbar;
	corr_matrix[2] Rho;
	vector<lower=0>[2] Sigma;
}
model{
	vector[1934] p;
	Sigma ~ exponential( 1 );
	Rho ~ lkj_corr( 2 );
	bbar ~ normal( 0 , 0.5 );
	abar ~ normal( 0 , 1 );
	{
		vector[2] YY[60];
		vector[2] MU;
		MU = [ abar , bbar ]';
    for ( j in 1:60 ) YY[j] = [ a[j] , b[j] ]';
		YY ~ multi_normal( MU , quad_form_diag(Rho , Sigma) );
	}
	for ( i in 1:1934 ) {
		p[i] = a[did[i]] + b[did[i]] * urban[i];
		p[i] = inv_logit(p[i]);
	}
	C ~ bernoulli( p );
}
"
