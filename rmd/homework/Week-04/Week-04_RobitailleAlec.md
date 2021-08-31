Alec L. Robitaille

# Homework: Week 4

2021-08-30 \[updated: 2021-08-30\]

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
```

## Question 1

> Consider three fictional Polynesian islands. On each there is a Royal
> Ornithologist charged by the king with surveying the birb population.
> They have each found the following proportions of 5 important birb
> species:

``` r
# Data
birds <- matrix(
    c(0.2, 0.2, 0.2, 0.2, 0.2,
        0.8, 0.1, 0.05, 0.025, 0.025,
        0.05, 0.15, 0.7, 0.05, 0.05),
    nrow = 3, ncol = 5, byrow = TRUE
)
dimnames(birds) <- list(as.character(1:3), LETTERS[1:5])
birds
```

    ##      A    B    C     D     E
    ## 1 0.20 0.20 0.20 0.200 0.200
    ## 2 0.80 0.10 0.05 0.025 0.025
    ## 3 0.05 0.15 0.70 0.050 0.050

> First, compute the entropy of each island’s birb distribution.
> Interpret these entropy values

``` r
DT <- melt(data.table(birds, keep.rownames = 'island'), id.vars = 'island',
                     variable.name = 'id', value.name = 'proportion')

# Entropy
entropy <- function(p) -sum(p * log(p))
DT[, .(entropy = entropy(proportion)), by = island]
```

    ##    island entropy
    ##    <char>   <num>
    ## 1:      1    1.61
    ## 2:      2    0.74
    ## 3:      3    0.98

The information entropy describes the uncertainty in a distribution of
probabilities given the average log-probability of an event (from
Statistical Rethinking 7.2). Island 1 has the highest entropy, with the
flat probability of 0.2 across 5 bird species. Island 2 has the lowest
entropy, including species A with the highest overall proportion 0.8.

``` r
divergence <- function(p, q) sum(p * (log(p) - log(q)))
z <- CJ(DT$island, DT$island, unique = TRUE)[, row_id := .I]
div <- z[, divergence(DT[island == V2, proportion], 
                                            DT[island == V1, proportion]), 
                 by = row_id]

matrix(div$V1, ncol = 3, nrow = 3, byrow = TRUE)
```

    ##      [,1] [,2] [,3]
    ## [1,] 0.00 0.87 0.63
    ## [2,] 0.97 0.00 1.84
    ## [3,] 0.64 2.01 0.00
