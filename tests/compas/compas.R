# load the COMPAS Pro-Publica data
{
  library(readr)
  compas <- read_csv("~/fairadapt/tests/compas-scores-two-years.csv")
  columns.keep <- which(names(compas)
                        %in% c("age", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count", "priors_count",
                               "c_charge_degree", "race", "two_year_recid")
                  )
  compas <- compas[, columns.keep]

  # factorise nicely
  compas$race <- factor(compas$race)
  levels(compas$race) <- c("Non-White", "Non-White", "White", "Non-White", "Non-White", "Non-White")
  compas$sex <- factor(compas$sex)
  compas$race <- factor(compas$race)
  compas$c_charge_degree <- factor(compas$c_charge_degree)
  compas <- as.data.frame(compas)
  compas$race <- relevel(compas$race, "White")
}


# explore whether C \ci A
{
  table(compas$sex, compas$race) # conclusion: minor discrepency (20% vs 30% female)

  x <- table(compas$race, compas$age)
  plot(x[2,] / sum(x[2,]), type = "b", col = "black", pch = 19)
  points(x[1,] / sum(x[1,]), type = "b", col = "blue", pch = 19) # conclusion: it is roughly OK
}


# train RF classifiers (20-fold)
reticulate::use_python("/anaconda3/bin/python3.7")
library(reticulate)
py_run_string("from importlib import reload")
source_python("reweighing_compas.py")
source_python("reductions_compas.py")

Probability_Predictions <- function(train.data, test.data, method) {
  label.col <- which(names(train.data) == "two_year_recid")
  attr.col <- which(names(train.data) == "race")
  test.base.indicator <- test.data[, attr.col] == "White"

  if (method == 1) {
    RF <- ranger::ranger(two_year_recid ~ ., data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data, predict.all = TRUE)$predictions
    Y.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]))
  }
  else if (method == 2) {
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

    # do fairadapt on the compas stuff
    L <- fairadapt::fairadapt(two_year_recid ~ ., train.data = train.data,
      test.data = test.data, protect.A = "race",
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(two_year_recid ~ ., data = adapted.train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
    Y.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]))
  }
  df <- data.frame(prob = Y.hat, race = as.integer(test.base.indicator))
  df$race <- factor(df$race)
  levels(df$race) <- c("Non-White", "White")
  return(df)
}


Acc_and_Gap <- function(train.data, test.data, method) {
  label.col <- which(names(train.data) == "two_year_recid")
  attr.col <- which(names(train.data) == "race")
  test.base.indicator <- test.data[, attr.col] == "White"

  if (method == 1) {
    RF <- ranger::ranger(two_year_recid ~ ., data = train.data, num.trees = 500,
                         classification = T)
    Y.hat <- predict(RF, data = test.data)$predictions
  }
  else if (method == 2) {
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

    # do fairadapt on the compas stuff
    L <- fairadapt::fairadapt(two_year_recid ~ ., train.data = train.data,
      test.data = test.data, protect.A = "race",
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(two_year_recid ~ ., data = adapted.train.data, num.trees = 500,
                         classification = T)
    Y.hat <- predict(RF, data = adapted.test.data)$predictions
  }
  else if (method == 3) {
    RF <- ranger::ranger(two_year_recid ~ . - race, data = train.data, num.trees = 500,
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

nmethods = 7
nrep = 20
res <- replicate(nmethods, NULL)
set.seed(13)
subsample <- replicate(nrep, sample(1:nrow(compas), round(floor(nrow(compas)) * 3 / 4)))
for(i in 1:nrep) {
  train <- subsample[, i]
  train.data <- compas[train, ]
  test.data <- compas[-train, ]
  print(i)
  for(m in 1:nmethods) {
    res[[m]] <- cbind(res[[m]], Acc_and_Gap(train.data, test.data, method = m))
    print(m)
    if (m > 4) { # if reductions is used, environment needs a reload
      py_run_string("moments = reload(moments)")
      py_run_string("red = reload(red)")
    }
  }
}

probs.NRF <- Probability_Predictions(test.data, train.data, method = 1)
probs.ARF <- Probability_Predictions(test.data, train.data, method = 2)

# plotting the method comparison
{
# make the list into a data-frame (using reduce)
df.matrix <- sapply(res, function(x) c(mean(x[1, ]),
                                       mean(x[1, ]) - sd(x[1, ]),
                                       mean(x[1, ]) + sd(x[1, ]),
                                       mean(x[2, ]),
                                       mean(x[2, ]) - sd(x[2, ]),
                                       mean(x[2, ]) + sd(x[2, ])))
df <- data.frame(t(df.matrix))
names(df) <- c("y", "ymin", "ymax", "x", "xmin", "xmax")
df1 <- cbind(df, shape = factor(c(1L,2L,3L,4L,5L,6L, 7L)), Method = rbind("Normal RF", "fairadapt + RF",
                                                                          "Unaware RF (drop A)", "Reweighing",
                                                                          "Reductions (eps 0.001)",
                                                                          "Reductions (eps 0.01)",
                                                                          "Reductions (eps 0.1)"))
override.shape <- c(4,8,15,16,17,18,21)
shape <- 15:21

root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
load(file.path(root, "tests", "compas", "compas.RData"))
# plotting the method comparison
{
  ggplot(data = df1, aes(x = x, y = y)) +
  geom_point(aes(shape = Method, color = Method), size = 5) +
  scale_shape_manual(values=c(15:20, 25)) +
 # scale_fill_discrete(labels = list( TeX('drek'), TeX('drek'), TeX('drek'), TeX('drek'), TeX('drek'), TeX('drek'), TeX('drek')))+
  geom_linerange(aes(ymin = ymin,ymax = ymax, color = Method)) +
  geom_errorbarh(aes(xmin = xmin,xmax = xmax, color = Method), height = 0) +
  xlab("parity gap") + ylab("accuracy") + ggtitle("COMPAS - comparison of method performances", subtitle = waiver()) +
  theme_bw() +
    theme(legend.position = c(0.8,0.4),
      legend.box.background = element_rect(colour = "black"),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16))
  ggsave(paste0(file.path(root, "..", "Article", "compas_plot"), ".png"),
    device = "png", width = 7.25, height = 5)
}

# plotting the change in positive outcome probability
{
  library(cowplot)
  library(ggplot2)
  p1 <- ggplot(probs.NRF, aes(prob, fill = race)) +
    geom_density(alpha = 0.5) +
    xlab("outcome probability") + ylab("density") + ggtitle(TeX('COMPAS - density of $\\mathit{P}(\\widehat{Y} = 1 | A = a)$ for $a \\in $ (Non-White,White) for normal RF'), subtitle = waiver()) +
    theme_bw() + theme(legend.position = c(0.8,0.65),
      legend.box.background = element_rect(colour = "black"),
      plot.title = element_text(size = 9))
  p2 <- ggplot(probs.ARF, aes(prob, fill = race)) +
    geom_density(alpha = 0.5) +
    xlab("outcome probability") + ylab("density") + ggtitle(TeX('COMPAS - density of $\\mathit{P}(\\widehat{Y} = 1 | A = a)$ for $a \\in $ (Non-White,White) \n for fairadapt + RF'), subtitle = waiver()) +
    theme_bw() + theme(legend.position = c(0.8,0.65),
      legend.box.background = element_rect(colour = "black"),
      plot.title = element_text(size = 9))

  plot_grid(p1, p2, ncol = 1L)

  ggsave(paste0(file.path(root, "..", "Article", "compas_density"), ".png"),
    device = "png", width = 5, height = 5)
}

# individual exploration for COMPAS
{
  lookup <- which(compas$sex == "Male" &
                  compas$age == 30 &
                  compas$race == "Non-White")
  lookup <- c(241, 646, 807, 1425, 1470)

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

  # do fairadapt on the compas stuff
  candid <- fairadapt::fairadapt(two_year_recid ~ ., train.data = train.data,
    test.data = test.data, protect.A = "race",
    adj.mat = adjacency.matrix)

  # rbind back the data
  optim <- rbind(candid[[1]], cbind(two_year_recid = 0, candid[[2]]))
  optim <- optim[order(c(train, setdiff(1:nrow(compas), train))), ]
  # chuck away columns
  keep <- c("juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count")
  optim <- optim[lookup, keep]

  # unflip optim

  cps <- compas[lookup, keep]

  cbind(optim, cps)
}
