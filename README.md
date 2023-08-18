
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [fairadapt](https://dplecko.github.io/fairadapt/)

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/dplecko/fairadapt/workflows/build/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Abuild)
[![R check
status](https://github.com/dplecko/fairadapt/workflows/check/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Acheck)
[![pkgdown build
status](https://github.com/dplecko/fairadapt/workflows/pkgdown/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Apkgdown)
[![covr
status](https://github.com/dplecko/fairadapt/workflows/coverage/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Acoverage)
[![Codecov test
coverage](https://codecov.io/gh/dplecko/fairadapt/branch/main/graph/badge.svg?token=8A0EL5N4RE)](https://app.codecov.io/gh/dplecko/fairadapt)
<!-- badges: end -->

The R package fairadapt is intended for removing bias from machine
learning algorithms. In particular, it implements the pre-processing
procedure described in [Plecko & Meinshausen,
2019](https://arxiv.org/abs/1911.06685) (all the code used for producing
the figures in the paper can be found in the `jmlr-paper` folder). The
main idea is to adapt the training and testing data in a way which
prevents any further training procedure from learning an undesired bias.
The package currently offers the pre-processing step, after which the
user can use the adapted data to train any classifier. However, some
caution on the training step is still advised, so for more involved
applications with resolving variables, the user should refer to the
original paper.

## Installation

You can install the released version of fairadapt from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fairadapt")
```

## Example

<!-- example could be expanded to show bias before correction -->
<!-- add plot of adj.mat? visualization of how data changed? -->

An example of how fairadapt can be used is demonstrated below on the UCI
Adult dataset.

``` r
# loading the package
library(fairadapt)

vars <- c("sex", "age", "native_country", "marital_status", "education_num",
          "workclass", "hours_per_week", "occupation", "income")

# initialising the adjacency matrix
adj.mat <- c(
  0, 0, 0, 1, 1, 1, 1, 1, 1, # sex
  0, 0, 0, 1, 1, 1, 1, 1, 1, # age
  0, 0, 0, 1, 1, 1, 1, 1, 1, # native_country
  0, 0, 0, 0, 1, 1, 1, 1, 1, # marital_status
  0, 0, 0, 0, 0, 1, 1, 1, 1, # education_num
  0, 0, 0, 0, 0, 0, 0, 0, 1, # workclass
  0, 0, 0, 0, 0, 0, 0, 0, 1, # hours_per_week
  0, 0, 0, 0, 0, 0, 0, 0, 1, # occupation
  0, 0, 0, 0, 0, 0, 0, 0, 0  # income
)

adj.mat <- matrix(adj.mat, nrow = length(vars), ncol = length(vars),
                  dimnames = list(vars, vars), byrow = TRUE)

# reading in the UCI Adult data
adult <- readRDS(
  system.file("extdata", "uci_adult.rds", package = "fairadapt")
)
n <- nrow(adult) / 2

mod <- fairadapt(income ~ ., 
                 train.data = head(adult[, vars], n = n),
                 test.data = tail(adult[, vars], n = n), 
                 prot.attr = "sex", adj.mat = adj.mat, 
                 res.vars = "hours_per_week")

adapt.train <- adaptedData(mod)
adapt.test  <- adaptedData(mod, train = FALSE)

summary(mod)
#> 
#> Call:
#> fairadapt(formula = income ~ ., prot.attr = "sex", adj.mat = adj.mat, 
#>     train.data = head(adult[, vars], n = n), test.data = tail(adult[, 
#>         vars], n = n), res.vars = "hours_per_week")
#> 
#> Protected attribute:                 sex
#> Protected attribute levels:          Female, Male
#> Adapted variables:                   marital_status, education_num, workclass, occupation, income
#> Resolving variables:                 hours_per_week, age, native_country
#> 
#> Number of training samples:          1000
#> Number of test samples:              1000
#> Quantile method:                     rangerQuants
#> 
#> Total variation (before adaptation): -0.2014
#> Total variation (after adaptation):  -0.01676
```
