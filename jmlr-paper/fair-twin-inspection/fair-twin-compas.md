
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Fair-twin inspection

**fairadapt** is a package intended to remove bias from machine learning
algorithms. In particular, it implements the pre-processing procedure
described in [Plecko &
Meinshausen, 2019](https://arxiv.org/abs/1911.06685) which transforms
the data into its fair version, based on the causal graph of the data.
One very useful property of **fairadapt** is the so called *fair-twin
inspection*. In particular, for each instance (individual) in the
dataset, the method computes attribute values the individual would have
obtained in a *fair-world*.

## Fair-twin inspection on COMPAS

We show an example of fair-twin inspection on the COMPAS dataset.

We first load the data:

``` r
# fair-twin inspection for COMPAS
library(fairadapt)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
data <- read.csv(file.path(root, "tests", "real-data", "compas", "compas-scores-two-years.csv"))
columns.keep <- which(names(data)
  %in% c("age", "sex", "juv_fel_count",
    "juv_misd_count", "juv_other_count", "priors_count",
    "c_charge_degree", "race", "two_year_recid")
)
data <- data[, columns.keep]
levels(data$race) <- c("Non-White", "Non-White", "White", "Non-White", "Non-White", "Non-White")
data$race <- relevel(data$race, "White")
train <- 1:5000
train.data <- data[train, ]
test.data <- data[-train, ]
```

We next construct the adjacency matrix of the causal graph and apply
**fairadapt**:

``` r
# set-up the adjacency matrix
adjacency.matrix <- array(0, dim = c(9, 9))
colnames(adjacency.matrix) <- c("age", "sex", "juv_fel_count",
  "juv_misd_count", "juv_other_count", "priors_count",
  "c_charge_degree", "race", "two_year_recid")
rownames(adjacency.matrix) <- colnames(adjacency.matrix)

# adding the edges to the matrix
adjacency.matrix[c("race", "sex", "age"), c("juv_fel_count", "juv_misd_count",
  "juv_other_count", "priors_count",
  "c_charge_degree", "two_year_recid")] <- 1
adjacency.matrix[c("juv_fel_count", "juv_misd_count", "juv_other_count"),
  c("priors_count", "c_charge_degree", "two_year_recid")] <- 1
adjacency.matrix["priors_count", c("c_charge_degree", "two_year_recid")] <- 1
adjacency.matrix["c_charge_degree", "two_year_recid"] <- 1

# apply fairadapt
transformed.data <- fairadapt::fairadapt(two_year_recid ~ ., train.data = train.data,
  test.data = test.data, protect.A = "race",
  adj.mat = adjacency.matrix)

# reconstruct the ordering of the data
transformed.data <- rbind(transformed.data[[1]], cbind(two_year_recid = 0, transformed.data[[2]]))
```

Finally, we select several individuals that are non-white and 30 years
old. We wish to inspect the values their *fair-twins* would have
obtained in a *fair world*:

``` r
# take several individuals
individuals <- c(241, 646, 807, 1425, 1470) # all Non-White, Age 30

# keep several interesting columns
rel.cols <- c("juv_fel_count", "juv_other_count", "priors_count")
res <- cbind(data[individuals, rel.cols], transformed.data[individuals, rel.cols])
names(res) <- c(rel.cols, paste0("fair-twin_", rel.cols))
res <- res[, c(1, 4, 2, 5, 3, 6)]
res
#>      juv_fel_count fair-twin_juv_fel_count juv_other_count
#> 241              0                       0               0
#> 646              0                       0               0
#> 807              0                       0               0
#> 1425             2                       0               0
#> 1470             1                       0               2
#>      fair-twin_juv_other_count priors_count fair-twin_priors_count
#> 241                          0            4                      3
#> 646                          0            8                      5
#> 807                          0           17                     11
#> 1425                         0           20                     13
#> 1470                         1           15                     10
```

Note how we can analyze discrimination removal on individual level. In
the COMPAS example, we can see the juvenile/prior counts individuals
would have obtained had they been white. This property might be very
useful for *justifying fair decisions* on an individual level.
