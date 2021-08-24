Week 2
================
Alec L. Robitaille
2021-08-24 \[updated: 2021-08-24\]

% overline short

## Question 1

The weights listed below were recorded in the !Kung census, but heights
were not recorded for these individuals. Provide predicted heights and
89% compatibility intervals for each of these individuals. That is, fill
in the table below, using model-based predictions.

    Individual, weight, expected height, 89% interval
    1, 45,,,
    2, 40,,,
    3, 65,,,
    4, 31,,,
    5, 53,,,

Model:

*h*<sub>*i*</sub> ∼ *N**o**r**m**a**l*(*μ*<sub>*i*</sub>,*σ*)
*μ*<sub>*i*</sub> = *α* + *β*(*x*<sub>*i*</sub>−*x̄*)
*α* ∼ *N**o**r**m**a**l*(178,20)
*β* ∼ *L**o**g* − *N**o**r**m**a**l*(0,1)
*σ* ∼ *U**n**i**f**o**r**m*(0,50)

``` r
library(rethinking)
data(Howell1)

d <- Howell1[Howell1$age >= 18,]
```
