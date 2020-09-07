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

UCIAdult <- read_csv("~/fairness/fairadapt/tests/real-data/adult/UCIAdult.csv")
UCIAdult <- data.table(UCIAdult, stringsAsFactors = T)

rm_cols <- c("X1", "marital_status_Not-Married", "workclass_Other/Unknown", "occupation_?",
  "race_White", "sex_Female", "native_country_Not-United-States", "income_<=50K")

UCIAdult <- one_hot(UCIAdult, cols = "auto")
UCIAdult <- UCIAdult[, !(names(UCIAdult) %in% rm_cols), with = F]

order_col <- c("sex_Male", "age", "native_country_United-States",
  "marital_status_Married", "educatoin_num")
order_col <- c(order_col, setdiff(names(UCIAdult), order_col))

UCIAdult <- UCIAdult[, order_col, with = FALSE]

set.seed(2020)
train <- sample(1:nrow(UCIAdult), round(0.75*nrow(UCIAdult)))

write.csv(UCIAdult[train], file = file.path(root, "tests", "PSCF", "data", "UCIAdult_train.csv"))
write.csv(UCIAdult[-train], file = file.path(root, "tests", "PSCF", "data", "UCIAdult_test.csv"))

root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))

# get fairadapt predictions on the test set
{

  data <- read.csv(file.path(root, "tests", "real-data", "adult", "UCIAdult.csv"))
  data <- data[, -1]
  data[, "sex"] <- factor(data[, "sex"], levels = c("Male","Female"))
  data[, "race"] <- NULL

  adjacency.matrix <- array(0, dim = c(ncol(data), ncol(data)))
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
  lvls <- c("Male", "Female")
  outcome <- "income"
  attribute <- "sex"

  train.data <- data[train, ]
  test.data <- data[-train, ]
}

system.time({
  L <- fairadapt::fairadapt(income ~ ., train.data = train.data,
    test.data = test.data, protect.A = attribute,
    res.vars = c("workclass", "occupation", "hours_per_week"),
    adj.mat = adjacency.matrix)
  adapted.train.data <- L[[1]]
  adapted.test.data <- L[[2]]

  # RF training step
  RF <- ranger::ranger(income ~ ., data = adapted.train.data, num.trees = 500,
    classification = T)
  Y.hat <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
  FA.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]))
})

{
  NRF <- ranger::ranger(income ~ ., data = train.data, num.trees = 500,
    classification = T)
  NRF.hat <- predict(NRF, data = test.data, predict.all = TRUE)$predictions
  NRF.hat <- sapply(1:nrow(NRF.hat), function(i) mean(NRF.hat[i, ]))-1
}

L_prob <- list(
  `normal RF` = NRF.hat,
  `Fairadapt + RF` = FA.hat-1
)

beta <- as.integer(c(0, 10, 100, 1000))

L_append <- lapply(beta, function(bet) {
  read.csv(file.path(root, "tests", "pscf", "pred", paste0("adult_pred", bet, ".csv")), header = F)[["V1"]]
})

names(L_append) <- paste0("PSCF $\\beta =$ ", beta)

L_prob <- c(L_prob, L_append)

df1 <- p_df(L_prob, as.integer(test.data$sex == "Male"), as.integer(test.data$income == ">50K"))

# plotting the Methods comparison
{

  p_adult <- ggplot(data = df1, aes(x = gap, y = auc)) +
    geom_point(aes(shape = Method, color = Method), size = 5) +
    #scale_shape_manual(values=c(15:20, 25)) +
    geom_linerange(aes(ymin = auc.min,ymax = auc.max, color = Method)) +
    geom_errorbarh(aes(xmin = gap.min, xmax = gap.max, color = Method), height = 0) +
    xlab("Parity gap") + ylab("Accuracy") + ggtitle("Fairadapt vs. PSCF - Adult dataset") +
    theme_bw() + scale_color_discrete(labels = unname(TeX(sort(names(L_prob))))) +
    scale_shape_discrete(labels = unname(TeX(sort(names(L_prob))))) +
    theme(legend.position = c(0.7, 0.4),
       legend.box.background = element_rect(colour = "black"),
       legend.title = element_text(size = 16),
       legend.text = element_text(size = 12),
       legend.text.align = 0.5,
       axis.text = element_text(size = 12),
       axis.title = element_text(size = 14),
       plot.title = element_text(size = 16))

  ggsave(file.path(root, "..", "..", "Article", "plots", paste0("fapscf_adult", ".png")),
    device = "png", width = 7.5, height = 5)
}
