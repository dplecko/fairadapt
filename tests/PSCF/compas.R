library(readr)
library(mltools)
library(data.table)
library(fairadapt)
library(ranger)
library(ggplot2)
library(cowplot)
library(latex2exp)

root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
source(file.path(root, "tests", "PSCF", "pscf-helpers.R"))

# load COMPAS data & write one-hot encoded
{

  data <- read.csv(file.path(root, "tests", "real-data", "compas", "compas-scores-two-years.csv"))

  data$juv_count <- data$juv_fel_count + data$juv_misd_count + data$juv_other_count

  columns.keep <- which(names(data)
    %in% c("age", "sex", "juv_count", "priors_count",
      "c_charge_degree", "race", "two_year_recid")
  )

  data <- data[, columns.keep]
  levels(data$race) <- c("Non-White", "Non-White", "White", "Non-White", "Non-White", "Non-White")
  data$race <- relevel(data$race, "White")


  adjacency.matrix <- array(0, dim = c(ncol(data), ncol(data)))
  colnames(adjacency.matrix) <- c("age", "sex", "juv_count", "priors_count",
    "c_charge_degree", "race", "two_year_recid")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)

  # adding the edges to the matrix
  adjacency.matrix[c("race", "sex", "age"), c("juv_count", "priors_count",
    "c_charge_degree", "two_year_recid")] <- 1
  adjacency.matrix[c("juv_count"),
    c("priors_count", "c_charge_degree", "two_year_recid")] <- 1
  adjacency.matrix["priors_count", c("c_charge_degree", "two_year_recid")] <- 1
  adjacency.matrix["c_charge_degree", "two_year_recid"] <- 1

  lvls <- c("White", "Non-White")
  outcome <- "two_year_recid"
  attribute <- "race"



  set.seed(2020)
  train <- sample(1:nrow(data), round(0.75*nrow(data)))

  train.data <- data[train, ]
  test.data <- data[-train, ]
  attr <- test.data[[attribute]] == lvls[1]
  compas <- one_hot(data.table::data.table(data))

  order_col <- c("race_White", "sex_Male", "age", "juv_count", "priors_count", "c_charge_degree_F", "two_year_recid")
  compas <- compas[, order_col, with = FALSE]

  write.csv(compas[train], file = file.path(root, "tests", "PSCF", "data", "compas_train.csv"))
  write.csv(compas[-train], file = file.path(root, "tests", "PSCF", "data", "compas_test.csv"))


}

# get PSCF predictions
#system('python tests/PSCF/pycode/compas.py')
#system2('python3', 'tests/PSCF/pycode/compas.py')

# get fairadapt predictions
system.time({
  L <- fairadapt::fairadapt(two_year_recid ~ ., train.data = train.data,
    test.data = test.data, protect.A = attribute,
    res.vars = c("c_charge_degree"),
    adj.mat = adjacency.matrix)
  adapted.train.data <- L[[1]]
  adapted.test.data <- L[[2]]
  # RF training step
  RF <- ranger::ranger(two_year_recid ~ ., data = adapted.train.data, num.trees = 500,
    classification = T)
  Y.hat <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
  FA.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]))
})

# get unconstrained predictions
{
  NRF <- ranger::ranger(two_year_recid ~ ., data = train.data, num.trees = 500,
    classification = T)
  NRF.hat <- predict(NRF, data = test.data, predict.all = TRUE)$predictions
  NRF.hat <- sapply(1:nrow(NRF.hat), function(i) mean(NRF.hat[i, ]))
}

L_prob <- list(
  `normal RF` = NRF.hat,
  `Fairadapt + RF` = FA.hat
)

beta <- as.integer(c(0, 10, 100, 1000))

L_append <- lapply(beta, function(bet) {
  read.csv(file.path(root, "tests", "PSCF", "pred", paste0("compas_pred", bet, ".csv")), header = F)[["V1"]]
})

names(L_append) <- paste0("PSCF $\\beta =$ ", beta)

L_prob <- c(L_prob, L_append)

df1 <- p_df(L_prob, as.integer(attr), as.integer(test.data$two_year_recid == 1))

# plotting the Methods comparison
{

  p_compas <- ggplot(data = df1, aes(x = gap, y = auc)) +
    geom_point(aes(shape = Method, color = Method), size = 5) +
    #scale_shape_manual(values=c(15:20, 25)) +
    geom_linerange(aes(ymin = auc.min,ymax = auc.max, color = Method)) +
    geom_errorbarh(aes(xmin = gap.min, xmax = gap.max, color = Method), height = 0) +
    xlab("Parity gap") + ylab("Accuracy") + ggtitle("Fairadapt vs. PSCF - COMPAS dataset") +
    theme_bw() + scale_color_discrete(labels = unname(TeX(sort(names(L_prob))))) +
    scale_shape_discrete(labels = unname(TeX(sort(names(L_prob))))) +
    theme(legend.position = c(0.25, 0.7),
      legend.box.background = element_rect(colour = "black"),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12),
      legend.text.align = 0.5,
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16))

  ggsave(file.path(root, "..", "..", "Article", "plots", paste0("fapscf_compas", ".png")),
    device = "png", width = 7.5, height = 5)
}
