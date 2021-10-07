library(conflicted)

library(targets)
library(stantargets)
library(tarchetypes)

library(bookdown)

library(ggplot2)
library(patchwork)

library(data.table)

library(rethinking)
library(tidybayes)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(boot)
library(loo)

library(ggdag)
library(dagitty)

library(rprojroot)


conflict_prefer('ess_bulk', 'posterior')
conflict_prefer('ess_tail', 'posterior')
conflict_prefer('mad', 'posterior')
conflict_prefer('rhat', 'posterior')
conflict_prefer('sd', 'posterior')
conflict_prefer('var', 'posterior')

conflict_prefer('logit', 'rethinking')
conflict_prefer('rstudent', 'rethinking')

conflict_prefer('loo', 'loo')
conflict_prefer('compare', 'loo')
