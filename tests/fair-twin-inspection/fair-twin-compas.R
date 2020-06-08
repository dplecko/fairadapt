# fair-twin inspection for COMPAS
library(fairadapt)
library(ggplot2)
library(cowplot)
library(latex2exp)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))

# take several individuals
individuals <- c(241, 646, 807, 1425, 1470) # all Non-White, Age 30

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

# keep several interesting columns
rel.cols <- c("juv_fel_count", "juv_other_count", "priors_count")

res <- cbind(data[individuals, rel.cols], transformed.data[individuals, rel.cols])
names(res) <- c(rel.cols, paste0("fair-twin_", rel.cols))
res <- res[, c(1, 4, 2, 5, 3, 6)]

