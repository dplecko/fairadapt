library(fairadapt)
library(ggplot2)
library(cowplot)
library(latex2exp)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))

# functionality
Probability_Predictions <- function(train.data, test.data, method,
  adjacency.matrix, outcome, attribute, base.level) {
  label.col <- which(names(train.data) == outcome)
  attr.col <- which(names(train.data) == attribute)
  test.base.indicator <- test.data[, attr.col] == base.level
  data.formula <- as.formula(eval(paste(outcome, "~ .")))

  if (method == 1) {
    RF <- ranger::ranger(data.formula, data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data, predict.all = TRUE)$predictions
    Y.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]) - 1)
  }
  else if (method == 2) {
    # apply fairadapt
    L <- fairadapt::fairadapt(income ~ ., train.data = train.data,
      test.data = test.data, protect.A = attribute,
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(data.formula, data = adapted.train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
    Y.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]) - 1)
  }
  df <- data.frame(prob = Y.hat, gender = as.integer(test.base.indicator))
  names(df) <- c("prob", "gender")
  df$gender <- factor(df$gender)
  levels(df$gender) <- c("Female", "Male")
  return(df)
}

Acc_and_Gap <- function(train.data, test.data, method,
  adjacency.matrix, outcome, attribute, base.level) {
  label.col <- which(names(train.data) == outcome)
  attr.col <- which(names(train.data) == attribute)
  test.base.indicator <- test.data[, attr.col] == base.level
  data.formula <- as.formula(eval(paste(outcome, "~ .")))
  if (method == 1) {
    RF <- ranger::ranger(data.formula ~ ., data = train.data, num.trees = 500,
                         classification = T)
    Y.hat <- predict(RF, data = test.data)$predictions
  }
  else if (method == 2) {
    # do fairadapt on the compas stuff
    L <- fairadapt::fairadapt(data.formula, train.data = train.data,
      test.data = test.data, protect.A = "race",
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(data.formula, data = adapted.train.data, num.trees = 500,
                         classification = T)
    Y.hat <- predict(RF, data = adapted.test.data)$predictions
  }
  else if (method == 3) {
    unaware.formula <- as.formula(eval(paste(outcome, "~ . -", attribute)))
    RF <- ranger::ranger(unaware.formula, data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data)$predictions
  }
  else if (method == 4) {
    # Reweighing by Kamiran & Calders
    Y.hat <- reweigh_and_predict(r_to_py(train.data), r_to_py(test.data))
  }
  else if (method == 5) {
    # Reductions approach Agarwal et. al.
    Y.hat <- reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.001)
  }
  else if (method == 6) {
    # Reductions approach Agarwal et. al.
    Y.hat <- reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.01)
  }
  else if (method == 7) {
    # Reductions approach Agarwal et. al.
    Y.hat <- reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.01)
  }

  return(c(sum(Y.hat == test.data[, label.col]) / nrow(test.data),
           abs(sum(Y.hat[test.base.indicator]) / sum(test.base.indicator)  -
               sum(Y.hat[!test.base.indicator]) / sum(!test.base.indicator)
              )
           )
        )
}

dataset <- "adult" # or "compas"

if (dataset == "adult") {
  data <- read.csv(file.path(root, "tests", "adult", "UCIAdult.csv"))
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


} else if (dataset == "compas") {
  data <- read.csv(file.path(root, "tests", "compas", "compas-scores-two-years.csv"))
  columns.keep <- which(names(data)
                        %in% c("age", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count", "priors_count",
                               "c_charge_degree", "race", "two_year_recid")
                  )
  data <- data[, columns.keep]
  levels(data$race) <- c("Non-White", "Non-White", "White", "Non-White", "Non-White", "Non-White")
  data$race <- relevel(data$race, "White")

  adjacency.matrix <- array(0, dim = c(ncol(data), ncol(data)))
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
}

# main()

# setup the connection to AIF360 (recticulate + python)
reticulate::use_python("/anaconda3/bin/python3.7")
library(reticulate)
py_run_string("from importlib import reload")
source_python(paste0("reweighing_", dataset, ".py"))
source_python(paste0("reductions_", dataset, ".py"))

nmethods = 7
nrep = 20
res <- replicate(nmethods, NULL)
set.seed(13)
subsample <- lapply(1:nrep, sample(1:nrow(data), round(floor(nrow(data)) * 3 / 4)))
for(i in 1:nrep) {
  train <- subsample[[i]]
  train.data <- data[train, ]
  test.data <- data[-train, ]
  for(m in 1:nmethods) {
    res[[m]] <- cbind(res[[m]], Acc_and_Gap(train.data, test.data, method = m))
    if (m > 4) { # if reductions is used, environment needs a reload
      py_run_string("moments = reload(moments)")
      py_run_string("red = reload(red)")
    }
  }
}


# plotting the Methods comparison
{
  df.matrix <- sapply(res, function(x) c(mean(x[1, ]),
    mean(x[1, ]) - sd(x[1, ]),
    mean(x[1, ]) + sd(x[1, ]),
    mean(x[2, ]),
    mean(x[2, ]) - sd(x[2, ]),
    mean(x[2, ]) + sd(x[2, ])))
  df <- data.frame(t(df.matrix))
  names(df) <- c("y", "ymin", "ymax", "x", "xmin", "xmax")
  df1 <- cbind(df, shape = factor(c(1L,2L,3L,4L,5L,6L, 7L)), Method = rbind("Normal RF", "fairadapt + RF", "Unaware RF (drop A)", "Reweighing",
    "Reductions (eps 0.001)",
    "Reductions (eps 0.01)",
    "Reductions (eps 0.1)"))

  ggplot(data = df1, aes(x = x, y = y)) +
    geom_point(aes(shape = Method, color = Method), size = 5) +
    scale_shape_manual(values=c(15:20, 25)) +
    geom_linerange(aes(ymin = ymin,ymax = ymax, color = Method)) +
    geom_errorbarh(aes(xmin = xmin,xmax = xmax, color = Method), height = 0) +
    xlab("parity gap") + ylab("accuracy") + ggtitle(paste0(dataset, " - comparison of method performances"), subtitle = waiver()) +
    theme_bw() +
    theme(legend.position = c(0.8,0.4),
      legend.box.background = element_rect(colour = "black"),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16))
  ggsave(paste0(file.path(root, "..", "Article", paste0(dataset, "_plot")), ".png"),
    device = "png", width = 7.25, height = 5)
}

# change in positive outcome probability under fairadapt
{
  probs.NRF <- Probability_Predictions(test.data, train.data, method = 1)
  probs.ARF <- Probability_Predictions(test.data, train.data, method = 2)
  p1 <- ggplot(probs.NRF, aes(prob, fill = gender)) +
    geom_density(alpha = 0.5) +
    xlab("outcome probability") + ylab("density") + ggtitle(TeX(paste0(dataset, ' - density of $\\mathit{P}(\\widehat{Y} = 1 | A = a)$ for $a \\in $ (Male, Female) for fairadapt + RF')), subtitle = waiver()) +
    theme_bw() + theme(legend.position = c(0.8, 0.65),
      legend.box.background = element_rect(colour = "black"),
      plot.title = element_text(size = 9))

  p2 <- ggplot(probs.ARF, aes(prob, fill = gender)) +
    geom_density(alpha = 0.5) +
    xlab("outcome probability") + ylab("density") + ggtitle(TeX(paste0(dataset, ' - density of $\\mathit{P}(\\widehat{Y} = 1 | A = a)$ for $a \\in $ (Male, Female) for fairadapt + RF')), subtitle = waiver()) +
    theme_bw() + theme(legend.position = c(0.8,0.65),
      legend.box.background = element_rect(colour = "black"),
      plot.title = element_text(size = 9))

  plot_grid(p1, p2, ncol = 1L)

  ggsave(paste0(file.path(root, "..", "Article", paste0(dataset, "_density")), ".png"),
    device = "png", width = 5, height = 5)
}
