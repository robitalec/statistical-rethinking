Week 1
================
Alec L. Robitaille
2021-08-18 \[updated: 2021-08-24\]

## Variables

N: fixed by experimenter

p: Prior probability

W: A probability distribution given the data.

## Joint model

W \~ Binomial(N , p)

p \~ Uniform(0, 1)

W is distributed binomially with N observations and probability p on
each

p is distributed uniformally at 1

## Question 1

Suppose the globe tossing data (Chapter 2) had turned out to be 8 water
in 15 tosses. Construct the posterior distribution, using grid
approximation. Use the same flat prior as in the book.

``` r
# Size of grid for grid approximation
gridsize <- 1000

# Prior grid
prior_grid <- seq(0, 1, length.out = gridsize)

# Prior probability (all 1)
prior_prob <- rep(1, gridsize)

# Data probability
#  given 4/15, using binomial distribution
data_prob <- dbinom(8, 15, prob = prior_grid)

# Calculate the posterior numerator by multiplying prior and data probability
posterior_num <- prior_prob * data_prob
# Standardize by sum of posterior numerator
posterior <- posterior_num / sum(posterior_num)

# Save for later
posterior_1 <- posterior

plot(posterior, type = 'l')
```

![](Week-01_RobitailleAlec_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# Sample from posterior
samples <- sample(prior_grid, size = gridsize, prob = posterior, replace = TRUE)
mean(samples)
```

    ## [1] 0.5227317

``` r
PI(samples, .99)
```

    ##        1%      100% 
    ## 0.2302302 0.8029079

## Question 2

Start over in 1, but now use a prior that is zero below p = 0.5 and a
constant above p = 0.5. This corresponds to prior information that a
majority of the Earth’s surface is water. What difference does the
better prior make?

``` r
# Size of grid for grid approximation
gridsize <- 1000

# Prior grid
prior_grid <- seq(0, 1, length.out = gridsize)

# Prior probability (all 1 above 0.5, all 0 below)
prior_prob <- c(rep(0, gridsize / 2), rep(1, gridsize / 2))

# Data probability
#  given 4/15, using binomial distribution
data_prob <- dbinom(4, 15, prob = prior_grid)

# Calculate the posterior numerator by multiplying prior and data probability
posterior_num <- prior_prob * data_prob
# Standardize by sum of posterior numerator
posterior <- posterior_num / sum(posterior_num)

# Save for later
posterior_2 <- posterior

plot(posterior, type = 'l')
```

![](Week-01_RobitailleAlec_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Sample from posterior
samples <- sample(prior_grid, size = gridsize, prob = posterior, replace = TRUE)
mean(samples)
```

    ## [1] 0.5522553

``` r
PI(samples, .99)
```

    ##        1%      100% 
    ## 0.5005005 0.7327327

Narrower curve, higher max, all zeroes before 0.5

## Question 3

For the posterior distribution from 2, compute 89% percentile and HPDI
intervals. Compare the widths of these intervals. Which is wider? Why?
If you had only the information in the interval, what might you
misunderstand about the shape of the posterior distribution?

``` r
library(rethinking)
library(ggplot2)
library(data.table)

# Calculate Percentile Interval at 89%
percent_interval <- PI(posterior, prob = 0.89)
percent_interval
```

    ##          5%         94% 
    ## 0.000000000 0.007254589

``` r
# Calculate Highest Posterior Density at 89%
highest_post_dens <- HPDI(posterior, prob = 0.89)
highest_post_dens
```

    ##       |0.89       0.89| 
    ## 0.000000000 0.002523398
