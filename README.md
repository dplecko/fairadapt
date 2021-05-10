
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
coverage](https://codecov.io/gh/dplecko/fairadapt/branch/master/graph/badge.svg?token=8A0EL5N4RE)](https://codecov.io/gh/dplecko/fairadapt)
<!-- badges: end -->

fairadapt is a package intended to remove bias from machine learning
algorithms. In particular, it implements the pre-processing procedure
described in [Plecko &
Meinshausen, 2019](https://arxiv.org/abs/1911.06685) (all the code used
for producing the figures in the paper can be found in the `jmlr-paper`
folder). The main idea is to adapt the training and testing data in a
way which prevents any further training procedure from learning an
undesired bias. The package currently offers the pre-processing step,
after which the user can use the adapted data to train any classifier.
However, some caution on the training step is still advised, so for more
involved applications with resolving variables, the user should refer to
the original paper.

## Installation

You can install the released version of fairadapt from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fairadapt")
```

## Example

An example of how fairadapt can be used is demonstrated below on the UCI
Adult dataset.

``` r
## loading the package
library(fairadapt)
## initialising the adjacency matrix
adjacency.matrix <- array(0, dim = c(9, 9))
colnames(adjacency.matrix) <- c("sex", "age", "native_country",
                                "marital_status", "educatoin_num",
                                "workclass", "hours_per_week", "occupation",
                                "income")
rownames(adjacency.matrix) <- colnames(adjacency.matrix)
# addition of edges
adjacency.matrix[c("sex","age","native_country"),
                 c("marital_status", "educatoin_num",
                   "workclass", "hours_per_week", "occupation",
                   "income")] <- 1
adjacency.matrix["marital_status",c("educatoin_num",
                                    "workclass", "hours_per_week", "occupation",
                                    "income")] <- 1
adjacency.matrix["educatoin_num", c("workclass", "hours_per_week",
                                    "occupation", "income")] <- 1
adjacency.matrix[c("workclass", "hours_per_week", "occupation"), "income"] <- 1

# reading in the UCI Adult data
adult <- readRDS(
  system.file("extdata", "uci_adult.rds", package = "fairadapt")
)

train.size <- n1
test.size <- n2
L <- fairadapt(income ~ ., train.data = adult[1:train.size, ],
               test.data = adult[train.size + 1:test.size, ], protect.A = "sex",
               adj.mat = adjacency.matrix, res.vars = "hours_per_week")
adapted.train.data <- L[[1]]
adapted.test.data <- L[[2]]
```
