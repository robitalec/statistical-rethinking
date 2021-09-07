Alec L. Robitaille

# Homework: Week 5

2021-09-03 \[updated: 2021-09-06\]

### Setup

``` r
# Packages
library(ggdag)
```

    ## 
    ## Attaching package: 'ggdag'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(dagitty)
library(data.table)
library(ggplot2)
library(tidybayes)

# Functions
dag_plot <- function(dag) {
    stat <- node_status(dag, FALSE)
    stat$data$status[is.na(stat$data$status)] <- 'intermediate'
    ggplot(stat, aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_point(aes(color = status), alpha = 0.5, size = 15) +
      geom_dag_edges() +
        labs(color = '') + 
      geom_dag_text(color = 'black') +
        scale_color_manual(values = list('exposure' = '#35608DFF',
                                                                         'outcome' = '#22A884FF',
                                                                         'intermediate' = 'grey50')) + 
      theme_void()
}

cmd_draws <- function(model) {
    as_draws_df(model$draws())
}
```

## Question 1

> Consider the `data(Wines2012)` data table. These data are expert
> ratings of 20 different French and American wines by 9 different
> French and American judges.

### Data

> Your goal is to model score, the subjective rating assigned by each
> judge to each wine. I recommend standardizing it. In this first
> problem, consider only variation among judges and wines. Construct
> index variables of judge and wine and then use these index variables
> to construct a linear regression model.

``` r
library(rethinking)
library(cmdstanr)
```

    ## This is cmdstanr version 0.4.0.9000

    ## - Online documentation and vignettes at mc-stan.org/cmdstanr

    ## - CmdStan path set to: /home/alecr/.cmdstan/cmdstan-2.27.0

    ## - Use set_cmdstan_path() to change the path

``` r
library(data.table)
library(posterior)
```

    ## This is posterior version 1.0.1

    ## 
    ## Attaching package: 'posterior'

    ## The following objects are masked from 'package:rstan':
    ## 
    ##     ess_bulk, ess_tail

    ## The following objects are masked from 'package:stats':
    ## 
    ##     mad, sd, var

``` r
library(bayesplot)
```

    ## This is bayesplot version 1.8.1

    ## - Online documentation and vignettes at mc-stan.org/bayesplot

    ## - bayesplot theme set to bayesplot::theme_default()

    ##    * Does _not_ affect other ggplot2 plots

    ##    * See ?bayesplot_theme_set for details on theme setting

    ## 
    ## Attaching package: 'bayesplot'

    ## The following object is masked from 'package:posterior':
    ## 
    ##     rhat

``` r
data("Wines2012")

DT <- data.table(Wines2012)

DT[, scale_score := scale(score)]
DT[, index_judge := .GRP, judge]
DT[, index_wine := .GRP, wine]

n_index_judge <- DT[, uniqueN(index_judge)]
n_index_wine <- DT[, uniqueN(index_wine)]
n_rows <- DT[, .N]
```

### Prior predictive simulation

``` r
register_knitr_engine(override = FALSE)
```

``` stan
data {
  int<lower=0> N;
  int<lower=0> N_judges;
  int<lower=0> N_wines;
}
parameters{
  real alpha;
  vector[N_judges] beta_judge;
  vector[N_wines] beta_wine;
  real<lower=0> sigma;
}
model{
  sigma ~ exponential(1);
  beta_wine ~ normal(0, 0.5);
  beta_judge~normal(0, 0.5);
  alpha~normal(0, 0.2);
}
```

``` r
# q1_stan <- 'q1_prior.stan'
# writeLines(readLines(q1_stan))
# q1_prior <- cmdstan_model(q1_stan)

q1_prior_sample <- q1_prior$sample(
    data = list(N = n_rows,
                            N_judges = n_index_judge,
                            N_wines = n_index_wine)
)
```

    ## Running MCMC with 4 sequential chains...
    ## 
    ## Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 1 finished in 0.1 seconds.
    ## Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 2 finished in 0.1 seconds.
    ## Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 3 finished in 0.1 seconds.
    ## Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 4 finished in 0.1 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 0.1 seconds.
    ## Total execution time: 0.6 seconds.

``` r
q1_prior_draws <- cmd_draws(q1_prior_sample)

mcmc_areas(q1_prior_draws, regex_pars = 'judge')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
mcmc_areas(q1_prior_draws, regex_pars = 'wine')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

> Justify your priors. You should end up with 9 judge parameters and 20
> wine parameters.

Given the parameters are scaled, the prior predictive plots show scaled
wine scores mostly between -2 and 2. Since we do not have any prior
knowledge about how this relationship (positive/negative slopes), we are
satisfied with this relatively conservative prior.

### Model

> Use ulam instead of quap to build this model, and be sure to check the
> chains for convergence. If you’d rather build the model directly in
> Stan or PyMC3, go ahead. I just want you to use Hamiltonian Monte
> Carlo instead of quadratic approximation.

``` stan
data {
  int<lower=0> N;
  int<lower=0> N_judges;
  int<lower=0> N_wines;
  int index_judge[N];
  int index_wine[N];
  vector[N] scale_score;
}
parameters{
  real alpha;
  vector[N_judges] beta_judge;
  vector[N_wines] beta_wine;
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
```

``` r
# q1_stan <- 'q1.stan'
# writeLines(readLines(q1_stan))
# q1_model <- cmdstan_model(q1_stan)

q1_sample <- q1_model$sample(
    data = list(N = n_rows,
                            N_judges = n_index_judge,
                            N_wines = n_index_wine,
                            scale_score = DT[, as.numeric(scale_score)],
                            index_judge = DT[, index_judge],
                            index_wine = DT[, index_wine]
                            )
)
```

    ## Running MCMC with 4 sequential chains...
    ## 
    ## Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling)

    ## Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 1 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/tmp/RtmpFYN8mB/model-a8a66084b51.stan', line 23, column 2 to column 34)

    ## Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 1

    ## Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 1 finished in 0.1 seconds.
    ## Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 2 finished in 0.1 seconds.
    ## Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 3 finished in 0.1 seconds.
    ## Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling)

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/tmp/RtmpFYN8mB/model-a8a66084b51.stan', line 23, column 2 to column 34)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 4 finished in 0.1 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 0.1 seconds.
    ## Total execution time: 0.9 seconds.

``` r
q1_draws <- cmd_draws(q1_sample)
setDT(q1_draws)
```

> How do you interpret the variation among individual judges and
> individual wines? Do you notice any patterns, just by plotting the
> differences? Which judges gave the highest/lowest ratings? Which wines
> were rated worst/ best on average?

``` r
# Judges
precis(q1_draws, depth = 2)[3:11,]
```

    ##                mean   sd   5.5%  94.5%       histogram
    ## beta_judge[1] -0.54 0.20 -0.857 -0.223        ▁▁▂▅▇▃▁▁
    ## beta_judge[2] -0.34 0.19 -0.647 -0.036        ▁▁▅▇▃▁▁▁
    ## beta_judge[3]  0.80 0.19  0.494  1.113  ▁▁▁▂▃▅▇▇▅▃▂▁▁▁
    ## beta_judge[4]  0.13 0.19 -0.172  0.440       ▁▁▁▃▇▅▁▁▁
    ## beta_judge[5] -0.66 0.19 -0.961 -0.350 ▁▁▁▁▂▅▇▇▇▅▂▁▁▁▁
    ## beta_judge[6]  0.48 0.20  0.169  0.785       ▁▁▁▅▇▃▁▁▁
    ## beta_judge[7] -0.28 0.20 -0.598  0.029 ▁▁▁▁▂▅▇▇▇▃▂▁▁▁▁
    ## beta_judge[8]  0.21 0.19 -0.099  0.513 ▁▁▁▁▂▅▇▇▅▃▂▁▁▁▁
    ## beta_judge[9]  0.22 0.20 -0.102  0.531  ▁▁▁▁▂▅▇▇▅▃▂▁▁▁

``` r
melt(q1_draws, measure.vars = patterns('beta_judge'))[, .(mean_score = mean(value)), variable][order(-mean_score)]
```

    ##         variable mean_score
    ##           <fctr>      <num>
    ## 1: beta_judge[3]       0.80
    ## 2: beta_judge[6]       0.48
    ## 3: beta_judge[9]       0.22
    ## 4: beta_judge[8]       0.21
    ## 5: beta_judge[4]       0.13
    ## 6: beta_judge[7]      -0.28
    ## 7: beta_judge[2]      -0.34
    ## 8: beta_judge[1]      -0.54
    ## 9: beta_judge[5]      -0.66

``` r
mcmc_areas(q1_draws, regex_pars = 'judge')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# Wines
precis(q1_draws, depth = 2)[12:31,]
```

    ##                  mean   sd   5.5% 94.5%    histogram
    ## beta_wine[1]   0.1128 0.25 -0.289  0.51 ▁▁▁▂▅▇▅▂▁▁▁▁
    ## beta_wine[2]   0.2209 0.26 -0.188  0.64  ▁▁▁▁▃▇▇▃▂▁▁
    ## beta_wine[3]  -0.1064 0.26 -0.516  0.31  ▁▁▁▂▅▇▅▂▁▁▁
    ## beta_wine[4]   0.2438 0.26 -0.161  0.66  ▁▁▁▃▇▇▅▂▁▁▁
    ## beta_wine[5]   0.0709 0.26 -0.349  0.49  ▁▁▁▂▇▇▅▂▁▁▁
    ## beta_wine[6]  -0.0157 0.27 -0.441  0.41   ▁▁▂▃▇▇▃▁▁▁
    ## beta_wine[7]  -0.0901 0.26 -0.508  0.32   ▁▁▁▂▅▇▇▂▁▁
    ## beta_wine[8]  -0.1880 0.26 -0.609  0.23   ▁▁▁▃▇▇▃▁▁▁
    ## beta_wine[9]  -0.1213 0.26 -0.528  0.29   ▁▁▂▅▇▅▂▁▁▁
    ## beta_wine[10] -0.1404 0.26 -0.559  0.28  ▁▁▁▃▇▇▅▂▁▁▁
    ## beta_wine[11]  0.0862 0.25 -0.325  0.49  ▁▁▁▂▅▇▅▂▁▁▁
    ## beta_wine[12]  0.4648 0.26  0.038  0.87    ▁▁▃▇▇▅▂▁▁
    ## beta_wine[13] -0.3085 0.26 -0.723  0.11  ▁▁▁▂▅▇▅▂▁▁▁
    ## beta_wine[14]  0.2276 0.25 -0.184  0.63   ▁▁▁▃▇▇▅▂▁▁
    ## beta_wine[15]  0.0992 0.26 -0.326  0.51  ▁▁▁▂▅▇▅▂▁▁▁
    ## beta_wine[16] -0.0320 0.27 -0.459  0.38   ▁▁▂▅▇▇▃▁▁▁
    ## beta_wine[17]  0.0034 0.26 -0.418  0.42   ▁▁▁▃▇▇▃▁▁▁
    ## beta_wine[18] -0.1674 0.25 -0.560  0.23   ▁▁▁▃▇▇▅▂▁▁
    ## beta_wine[19] -0.7161 0.26 -1.122 -0.31  ▁▁▁▂▅▇▅▂▁▁▁
    ## beta_wine[20]  0.3177 0.26 -0.089  0.72  ▁▁▂▅▇▅▂▁▁▁▁

``` r
melt(q1_draws, measure.vars = patterns('beta_wine'))[, .(mean_score = mean(value)), variable][order(-mean_score)]
```

    ##          variable mean_score
    ##            <fctr>      <num>
    ##  1: beta_wine[12]     0.4648
    ##  2: beta_wine[20]     0.3177
    ##  3:  beta_wine[4]     0.2438
    ##  4: beta_wine[14]     0.2276
    ##  5:  beta_wine[2]     0.2209
    ##  6:  beta_wine[1]     0.1128
    ##  7: beta_wine[15]     0.0992
    ##  8: beta_wine[11]     0.0862
    ##  9:  beta_wine[5]     0.0709
    ## 10: beta_wine[17]     0.0034
    ## 11:  beta_wine[6]    -0.0157
    ## 12: beta_wine[16]    -0.0320
    ## 13:  beta_wine[7]    -0.0901
    ## 14:  beta_wine[3]    -0.1064
    ## 15:  beta_wine[9]    -0.1213
    ## 16: beta_wine[10]    -0.1404
    ## 17: beta_wine[18]    -0.1674
    ## 18:  beta_wine[8]    -0.1880
    ## 19: beta_wine[13]    -0.3085
    ## 20: beta_wine[19]    -0.7161
    ##          variable mean_score

``` r
mcmc_areas(q1_draws, regex_pars = 'wine')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

Accounting for judges, most wines are scored with similar distributions.
Wine 19 however was particularly poorly scored. Judges, however, have
much more variable scoring with three individuals (judges 1, 2, 5)
entirely or almost entirely scoring lower than other judges. The worst
scoring judge was judge 5.

## Question 2

> Now consider three features of the wines and judges: (1) flight:
> Whether the wine is red or white. (2) wine.amer: Indicator variable
> for American wines. (3) judge.amer: Indicator variable for American
> judges. Use indicator or index variables to model the influence of
> these features on the scores. Omit the individual judge and wine index
> variables from Problem 1. Do not include interaction effects yet.
> Again use ulam, justify your priors, and be sure to check the chains.

### Data

``` r
DT[, scale_score := scale(score)]
DT[, index_flight := .GRP, flight]
DT[, index_wine_american := .GRP, wine.amer]
DT[, index_judge_american := .GRP, judge.amer]

n_index_flight <- DT[, uniqueN(flight)]
n_index_wine_american <- DT[, uniqueN(index_wine_american)]
n_index_judge_american <- DT[, uniqueN(index_judge_american)]
n_rows <- DT[, .N]

q2_data <- c(
    as.list(DT[, .(scale_score = as.numeric(scale_score),
                                 index_flight,
                                 index_wine_american,
                                 index_judge_american)]),
    N_flights = n_index_flight,
    N_wine_american = n_index_wine_american,
    N_judge_american = n_index_judge_american,
    N = n_rows
)
```

### Priors

``` stan
data {
  int<lower=0> N;
  int<lower=0> N_flights;
  int<lower=0> N_wine_american;
  int<lower=0> N_judge_american;
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
}
```

``` r
# q2_stan <- 'q2_prior.stan'
# writeLines(readLines(q2_stan))
# q2_prior <- cmdstan_model(q2_stan)

q2_prior_sample <- q2_prior$sample(data = q2_data[c('N_flights',
                                                                                                        'N_wine_american',
                                                                                                        'N_judge_american',
                                                                                                        'N')])
```

    ## Running MCMC with 4 sequential chains...
    ## 
    ## Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 1 finished in 0.0 seconds.
    ## Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 2 finished in 0.0 seconds.
    ## Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 3 finished in 0.0 seconds.
    ## Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 4 finished in 0.0 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 0.0 seconds.
    ## Total execution time: 0.5 seconds.

``` r
q2_prior_draws <- cmd_draws(q2_prior_sample)

mcmc_areas(q2_prior_draws, regex_pars = 'flights')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
mcmc_areas(q2_prior_draws, regex_pars = 'judge')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
mcmc_areas(q2_prior_draws, regex_pars = 'wine')
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

### Model

``` stan
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
```

``` r
# q2_stan <- 'q2.stan'
# writeLines(readLines(q2_stan))
# q2_model <- cmdstan_model(q2_stan)

q2_sample <- q2_model$sample(data = q2_data)
```

    ## Running MCMC with 4 sequential chains...
    ## 
    ## Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 1 finished in 0.4 seconds.
    ## Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 2 finished in 0.4 seconds.
    ## Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 3 finished in 0.4 seconds.
    ## Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup)

    ## Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:

    ## Chain 4 Exception: normal_lpdf: Scale parameter is 0, but must be positive! (in '/tmp/RtmpFYN8mB/model-a8a6743a692f.stan', line 29, column 2 to column 34)

    ## Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,

    ## Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.

    ## Chain 4

    ## Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 4 finished in 0.4 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 0.4 seconds.
    ## Total execution time: 1.7 seconds.

``` r
q2_draws <- cmd_draws(q2_sample)

q2_sample$summary()
```

    ## # A tibble: 9 × 10
    ##   variable            mean   median     sd    mad      q5     q95  rhat ess_bulk
    ##   <chr>              <dbl>    <dbl>  <dbl>  <dbl>   <dbl>   <dbl> <dbl>    <dbl>
    ## 1 lp__            -9.24e+1 -9.21e+1 2.06   1.89   -96.2   -89.7    1.00    1380.
    ## 2 alpha           -1.94e-3 -1.37e-3 0.208  0.199   -0.351   0.344  1.00    2950.
    ## 3 beta_flights[1] -4.28e-3 -2.64e-3 0.299  0.294   -0.499   0.481  1.00    2206.
    ## 4 beta_flights[2] -4.97e-3 -4.96e-3 0.297  0.295   -0.486   0.490  1.00    2186.
    ## 5 beta_wine_amer… -7.70e-2 -8.39e-2 0.300  0.305   -0.560   0.422  1.00    2379.
    ## 6 beta_wine_amer…  1.08e-1  1.03e-1 0.301  0.303   -0.389   0.604  1.00    2391.
    ## 7 beta_judge_ame… -1.23e-1 -1.27e-1 0.307  0.300   -0.623   0.386  1.00    2424.
    ## 8 beta_judge_ame…  1.16e-1  1.13e-1 0.305  0.303   -0.373   0.624  1.00    2299.
    ## 9 sigma            9.99e-1  9.96e-1 0.0534 0.0509   0.913   1.09   1.00    3122.
    ## # … with 1 more variable: ess_tail <dbl>

``` r
mcmc_trace(q2_draws)
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
# Recall: 
DT[, .N, .(flight, index_flight)]
```

    ##    flight index_flight     N
    ##    <fctr>        <int> <int>
    ## 1:  white            1    90
    ## 2:    red            2    90

``` r
DT[, .N, .(judge.amer, index_judge_american)]
```

    ##    judge.amer index_judge_american     N
    ##         <int>                <int> <int>
    ## 1:          0                    1    80
    ## 2:          1                    2   100

``` r
DT[, .N, .(wine.amer, index_wine_american)]
```

    ##    wine.amer index_wine_american     N
    ##        <int>               <int> <int>
    ## 1:         1                   1   108
    ## 2:         0                   2    72

``` r
labs <- c(
    'beta_flights[1]' = 'White Wine',
    'beta_flights[2]' = 'Red Wine',
    'beta_wine_american[1]' = 'French Wine',
    'beta_wine_american[2]' = 'American Wine',
    'beta_judge_american[1]' = 'American Judge',
    'beta_judge_american[2]' = 'French Judge'
)
mcmc_areas(q2_draws, regex_pars = 'flight') + scale_y_discrete(labels = labs)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
mcmc_areas(q2_draws, regex_pars = 'judge') + scale_y_discrete(labels = labs)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
mcmc_areas(q2_draws, regex_pars = 'wine') + scale_y_discrete(labels = labs)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

<!-- > What do you conclude about the differences among the wines and judges? Try to -->
<!-- relate the results to the inferences in Problem 1 -->

## Question 3

> Now consider two-way interactions among the three features. You should
> end up with three different interaction terms in your model. These
> will be easier to build, if you use indicator variables. Again use
> ulam, justify your priors, and be sure to check the chains. Explain
> what each interaction means. Be sure to interpret the model’s
> predictions on the outcome scale (mu, the expected score), not on the
> scale of individual parameters. You can use link to help with this, or
> just use your knowledge of the linear model instead. What do you
> conclude about the features and the scores? Can you relate the results
> of your model(s) to the individual judge and wine inferences from
> Problem 1?

### Data

``` r
DT[, index_interactions := .GRP, .(judge.amer, wine.amer, flight)]
n_index_interactions <- DT[, uniqueN(index_interactions)]

q3_data <- c(
    as.list(DT[, .(scale_score = as.numeric(scale_score),
                                 index_interactions)]),
    N_interactions = n_index_interactions,
    N = n_rows
)
```

### Model

``` stan
data {
  int<lower=0> N;
  int<lower=0> N_interactions;

  int index_interactions[N];

  vector[N] scale_score;
}
parameters{
  real alpha;
  vector[N_interactions] beta_interactions;
  real<lower=0> sigma;
}
model{
  alpha ~ normal(0, 0.2);
  beta_interactions ~ normal(0, 0.25);
  sigma ~ exponential(1);

  vector[N] mu;
  mu = alpha + beta_interactions[index_interactions];

  scale_score ~ normal(mu, sigma);
}
```

``` r
q3_stan <- 'q3.stan'
writeLines(readLines(q3_stan))
```

    ## data {
    ##   int<lower=0> N;
    ##   int<lower=0> N_interactions;
    ## 
    ##   int index_interactions[N];
    ## 
    ##   vector[N] scale_score;
    ## }
    ## parameters{
    ##   real alpha;
    ##   vector[N_interactions] beta_interactions;
    ##   real<lower=0> sigma;
    ## }
    ## model{
    ##   alpha ~ normal(0, 0.2);
    ##   beta_interactions ~ normal(0, 0.25);
    ##   sigma ~ exponential(1);
    ## 
    ##   vector[N] mu;
    ##   mu = alpha + beta_interactions[index_interactions];
    ## 
    ##   scale_score ~ normal(mu, sigma);
    ## }

``` r
q3_model <- cmdstan_model(q3_stan)

q3_sample <- q3_model$sample(data = q3_data)
```

    ## Running MCMC with 4 sequential chains...
    ## 
    ## Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 1 finished in 0.1 seconds.
    ## Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 2 finished in 0.1 seconds.
    ## Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 3 finished in 0.1 seconds.
    ## Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
    ## Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
    ## Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
    ## Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
    ## Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
    ## Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
    ## Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
    ## Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
    ## Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
    ## Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
    ## Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
    ## Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
    ## Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
    ## Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
    ## Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
    ## Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
    ## Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
    ## Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
    ## Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
    ## Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
    ## Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
    ## Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
    ## Chain 4 finished in 0.1 seconds.
    ## 
    ## All 4 chains finished successfully.
    ## Mean chain execution time: 0.1 seconds.
    ## Total execution time: 0.5 seconds.

``` r
q3_draws <- cmd_draws(q3_sample)

q3_sample$summary()
```

    ## # A tibble: 11 × 10
    ##    variable          mean   median     sd    mad      q5      q95  rhat ess_bulk
    ##    <chr>            <dbl>    <dbl>  <dbl>  <dbl>   <dbl>    <dbl> <dbl>    <dbl>
    ##  1 lp__          -9.25e+1 -9.22e+1 2.27   2.18   -96.7   -89.4     1.00    1638.
    ##  2 alpha          1.47e-3  2.30e-3 0.0999 0.0988  -0.164   0.164   1.00    2920.
    ##  3 beta_interac…  2.75e-2  2.96e-2 0.167  0.170   -0.246   0.298   1.00    5072.
    ##  4 beta_interac… -1.90e-1 -1.91e-1 0.183  0.176   -0.480   0.117   1.00    5666.
    ##  5 beta_interac…  2.52e-2  2.39e-2 0.159  0.162   -0.230   0.292   1.00    4743.
    ##  6 beta_interac…  1.09e-1  1.11e-1 0.173  0.174   -0.170   0.392   1.00    5591.
    ##  7 beta_interac…  1.23e-1  1.24e-1 0.182  0.182   -0.169   0.425   1.00    6494.
    ##  8 beta_interac… -2.52e-1 -2.53e-1 0.167  0.167   -0.520   0.0164  1.00    5214.
    ##  9 beta_interac…  1.78e-1  1.78e-1 0.176  0.181   -0.109   0.467   1.00    5601.
    ## 10 beta_interac… -1.42e-2 -1.46e-2 0.164  0.169   -0.281   0.256   1.00    5465.
    ## 11 sigma          9.92e-1  9.90e-1 0.0540 0.0545   0.907   1.08    1.00    6383.
    ## # … with 1 more variable: ess_tail <dbl>

``` r
mcmc_trace(q3_draws)
```

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
DT[, judge_char := ifelse(judge.amer == 0, 'French Judge', 'American Judge')]
DT[, wine_char := ifelse(wine.amer == 0, 'French Wine', 'American Wine')]

labs <- DT[, .(.GRP, paste(.BY, collapse = ', ')), by = .(wine_char, judge_char, as.character(flight))][, .(GRP, V2)]
labs <- setNames(labs$V2, paste0('beta_interactions[', labs$GRP, ']'))
mcmc_areas(q3_draws, regex_pars = 'interaction') + scale_y_discrete(labels = labs)
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

![](Week-05_RobitailleAlec_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
