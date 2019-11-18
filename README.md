
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fairadapt

fairadapt is a package intended to remove bias from machine learning
algorithms. In particular, it implements the pre-processing procedure
described in [Plecko &
Meinshausen, 2019](https://arxiv.org/abs/1911.06685). The main idea is
to adapt the training and testing data in a way which prevents any
further training procedure from learning an undesired bias. The package
currently offers the pre-processing step, after which the user can use
the adapted data to train any classifier. However, some caution on the
training step is still advised, so for more involved applications with
resolving variables, the user should refer to the original paper.

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
adult <- read.csv("./tests/adult/UCIAdult.csv")
adult[, c("X", "race")] <- NULL
adult[, "sex"] <- factor(adult[, "sex"], levels = c("Male","Female"))
train.size <- n1
test.size <- n2
L <- fairadapt(income ~ ., train.data = adult[1:train.size, ],
               test.data = adult[train.size + 1:test.size, ], protect.A = "sex",
               adj.mat = adjacency.matrix, res.vars = "hours_per_week")
adapted.train.data <- L[[1]]
adapted.test.data <- L[[2]]
```
