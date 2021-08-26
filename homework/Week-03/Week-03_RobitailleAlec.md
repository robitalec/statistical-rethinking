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

dag <- dagify(
    weight ~ groupsize + avgfood,
    groupsize ~ avgfood,
    avgfood ~ area,
    exposure = 'area',
    outcome = 'weight'
)

dag_plot(dag)
```

![](Week-03_RobitailleAlec_files/figure-gfm/dag-1.png)<!-- -->

``` r
dag_paths(dag)  
```

    ## # A DAG with 4 nodes and 8 edges
    ## #
    ## # Exposure: area
    ## # Outcome: weight
    ## #
    ## # A tibble: 10 × 10
    ##    set   name          x     y direction to         xend  yend circular path    
    ##    <chr> <chr>     <dbl> <dbl> <fct>     <chr>     <dbl> <dbl> <lgl>    <chr>   
    ##  1 1     area       15.6  17.5 ->        avgfood    14.7  16.7 FALSE    open pa…
    ##  2 1     avgfood    14.7  16.7 ->        groupsize  13.6  16.3 FALSE    open pa…
    ##  3 1     avgfood    14.7  16.7 ->        weight     14.2  15.6 FALSE    <NA>    
    ##  4 1     groupsize  13.6  16.3 ->        weight     14.2  15.6 FALSE    open pa…
    ##  5 1     weight     14.2  15.6 <NA>      <NA>       NA    NA   FALSE    open pa…
    ##  6 2     area       15.6  17.5 ->        avgfood    14.7  16.7 FALSE    open pa…
    ##  7 2     avgfood    14.7  16.7 ->        groupsize  13.6  16.3 FALSE    <NA>    
    ##  8 2     avgfood    14.7  16.7 ->        weight     14.2  15.6 FALSE    open pa…
    ##  9 2     groupsize  13.6  16.3 ->        weight     14.2  15.6 FALSE    <NA>    
    ## 10 2     weight     14.2  15.6 <NA>      <NA>       NA    NA   FALSE    open pa…
