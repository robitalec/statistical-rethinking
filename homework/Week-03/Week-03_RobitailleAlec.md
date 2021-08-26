Week 3
================
Alec L. Robitaille
2021-08-25 \[updated: 2021-08-25\]

# Overview

All three problems below are based on the same data. The data in
`data(foxes)` are 116 foxes from 30 different urban groups in England.
These foxes are like street gangs. Group size varies from 2 to 8
individuals. Each group maintains its own (almost exclusive) urban
territory. Some territories are larger than others. The area variable
encodes this information. Some territories also have more avgfood than
others. We want to model the weight of each fox. For the problems below,
assume this DAG

## Question 1

Setup DAG

``` r
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

dag_plot <- function(dag) {
    ggplot(dag, aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_dag_point(color = 'grey', alpha = 0.8, size = 15) +
      geom_dag_edges() +
      geom_dag_text(color = 'black') +
      theme_void()
}

dag <- dagify(
    weight ~ groupsize + avgfood,
    groupsize ~ avgfood,
    avgfood ~ area
)

dag_plot(dag)
```

![](Week-03_RobitailleAlec_files/figure-gfm/dag-1.png)<!-- -->
