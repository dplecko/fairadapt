devtools::install(pkg = "..")

data <- read.csv("UCIAdult.csv")
data <- data[, -1]
data[, "sex"] <- factor(data[, "sex"], levels = c("Male","Female"))


Probability_Predictions <- function(train.data, test.data, method) {
  label.col <- which(names(train.data) == "income")
  attr.col <- which(names(train.data) == "sex")
  test.base.indicator <- test.data[, attr.col] == "Male"

  if (method == 1) {
    RF <- ranger::ranger(income ~ ., data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data, predict.all = TRUE)$predictions
    Y.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]) - 1)
  }
  else if (method == 2) {
    # set-up the adjacency matrix
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

    # do fairadapt on the compas stuff
    L <- fairadapt::FairAdapt(income ~ ., train.data = train.data,
      test.data = test.data, protect.A = "sex",
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(income ~ ., data = adapted.train.data, num.trees = 500,
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

Acc_and_Gap <- function(train.data, test.data, method) {
  label.col <- which(names(train.data) == "income")
  attr.col <- which(names(train.data) == "sex")
  test.base.indicator <- test.data[, attr.col] == "Male"

  if (method == 1) {
    RF <- ranger::ranger(income ~ ., data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data)$predictions
  }
  else if (method == 2) {
    # set-up the adjacency matrix
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

    # do fairadapt on the compas stuff
    L <- fairadapt::FairAdapt(income ~ ., train.data = train.data,
      test.data = test.data, protect.A = "sex",
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(income ~ ., data = adapted.train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = adapted.test.data)$predictions
  }
  else if (method == 3) {
    RF <- ranger::ranger(income ~ . - sex, data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data)$predictions
  }
  else if (method == 4) {
    # Reweighing by Kamiran & Calders
    Y.hat <- as.factor(reweigh_and_predict(r_to_py(train.data), r_to_py(test.data)))
    levels(Y.hat) <- c("<=50K", ">50K")
  }
  else if (method == 5) {
    # Reductions approach Agarwal et. al.
    Y.hat <- as.factor(reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.001))
    levels(Y.hat) <- c("<=50K", ">50K")
  }
  else if (method == 6) {
    # Reductions approach Agarwal et. al.
    Y.hat <- as.factor(reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.01))
    levels(Y.hat) <- c("<=50K", ">50K")
  }
  else if (method == 7) {
    # Reductions approach Agarwal et. al.
    Y.hat <- as.factor(reduce_and_predict(r_to_py(train.data), r_to_py(test.data), 0.1))
    levels(Y.hat) <- c("<=50K", ">50K")
  }

  return(c(sum(Y.hat == test.data[, label.col]) / nrow(test.data),
           abs(sum(as.integer(Y.hat[test.base.indicator])) / sum(test.base.indicator)  -
               sum(as.integer(Y.hat[!test.base.indicator])) / sum(!test.base.indicator)
              )
          )
        )
}

NRF <- ARF <- URF <- NULL
nrep <- 20
set.seed(12345)
subsample <- replicate(nrep, sample(1:nrow(data), round(floor(nrow(data)) * 3 / 4)))
for(i in 1:20) {
  train <- subsample[, i]
  train.data <- data[train, ]
  test.data <- data[-train, ]
  print(i)
  NRF <- cbind(NRF, Acc_and_Gap(train.data, test.data, method = 1))
  print("NRF done")
  ARF <- cbind(ARF, Acc_and_Gap(train.data, test.data, method = 2))
  print("ARF done")
  URF <- cbind(URF, Acc_and_Gap(train.data, test.data, method = 3))
  print("URF done")
}

probs.NRF <- Probability_Predictions(test.data, train.data, method = 1)
probs.ARF <- Probability_Predictions(test.data, train.data, method = 2)

setwd("~/fairadapt/tests")
save.image(file = "adult.RData")
quit()

# python methods to be run locally

load("adult.RData")
reticulate::use_python("/anaconda3/bin/python3.7")
library(reticulate)
py_run_string("from importlib import reload")
source_python("reweighing_adult.py")
source_python("reductions_adult.py")

nmethods = 7
nrep = 20
res <- replicate(nmethods, NULL)
set.seed(12345)
subsample <- replicate(nrep, sample(1:nrow(data), round(floor(nrow(data)) * 3 / 4)))
for(i in 1:nrep) {
  train <- subsample[, i]
  train.data <- data[train, ]
  test.data <- data[-train, ]
  print(i)
  for(m in 3:nmethods) {
    res[[m]] <- cbind(res[[m]], Acc_and_Gap(train.data, test.data, method = m))
    print(m)
    if (m > 4) { # if reductions is used, environment needs a reload
      py_run_string("moments = reload(moments)")
      py_run_string("red = reload(red)")
    }
  }
}
res[[1]] <- NRF
res[[2]] <- ARF
res[[3]] <- URF

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
  df1 <- cbind(df, shape = factor(c(1L,2L,3L,4L,5L,6L, 7L)), Method = rbind("Normal RF", "fairadapt + RF", "Unaware RF", "Reweighing",
    "Reductions (eps 0.001)",
    "Reductions (eps 0.01)",
    "Reductions (eps 0.1)"))

  ggplot(data = df1, aes(x = x, y = y)) +
    geom_point(aes(shape = Method, color = Method), size = 5) +
    scale_shape_manual(values=c(15:20, 25)) +
    #scale_color_discrete(labels = unname(TeX(c("drek","drek","drek","drek","drek","drek","drek"))))+
    geom_linerange(aes(ymin = ymin,ymax = ymax, color = Method)) +
    geom_errorbarh(aes(xmin = xmin,xmax = xmax, color = Method), height = 0) +
    xlab("parity gap") + ylab("accuracy") + ggtitle("Adult - comparison of method performances", subtitle = waiver()) +
    theme(legend.position = c(0.8,0.2), legend.text = element_text(size = 20),
      legend.title = element_text(size = 24, face = "bold"), axis.text=element_text(size=16),
      axis.title=element_text(size=20,face="bold"), plot.title=element_text(size=20, face = "bold"))


}

# plotting the change in positive outcome probability
{
  library(gridExtra)
  library(ggplot2)
  p1 <- ggplot(probs.NRF, aes(prob, fill = gender)) +
    geom_density(alpha = 0.5) +
    xlab("outcome probability") + ylab("density") + ggtitle(TeX('Adult - density of $\\mathit{P}(\\widehat{Y} = 1 | A = a)$ for $a \\in $ (Male, Female) for normal RF'), subtitle = waiver()) +
    theme(legend.position = c(0.8,0.8), legend.text = element_text(size = 20),
      legend.title = element_text(size = 24, face = "bold"), axis.text=element_text(size=16),
      axis.title=element_text(size=20,face="bold"), plot.title=element_text(size=20, face = "bold"))
  p2 <- ggplot(probs.ARF, aes(prob, fill = gender)) +
    geom_density(alpha = 0.5) +
    xlab("outcome probability") + ylab("density") + ggtitle(TeX('Adult - density of $\\mathit{P}(\\widehat{Y} = 1 | A = a)$ for $a \\in $ (Male, Female) for fairadapt + RF'), subtitle = waiver()) +
    theme(legend.position = c(0.8,0.8), legend.text = element_text(size = 20),
      legend.title = element_text(size = 24, face = "bold"), axis.text=element_text(size=16),
      axis.title=element_text(size=20,face="bold"), plot.title=element_text(size=20, face = "bold"))
  grid.arrange(p1, p2, ncol = 1)
}
