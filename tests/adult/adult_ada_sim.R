#devtools::install(pkg = ".")

data <- read.csv("UCIAdult.csv")
data <- data[, -1]
data[, "sex"] <- factor(data[, "sex"], levels = c("Male","Female"))

Acc_and_Gap <- function(train.data, test.data, method) {
  label.col <- which(names(train.data) == "income")
  attr.col <- which(names(train.data) == "sex")
  test.base.indicator <- test.data[, attr.col] == "Male"

  if (method == 1) {
    RF <- ranger::ranger(income ~ ., data = train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = test.data)$predictions
  }
  else if (method == "ARF") {
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
    L <- fairadapt::fairadapt(income ~ ., train.data = train.data,
      test.data = test.data, protect.A = "sex",
      adj.mat = adjacency.matrix)
    adapted.train.data <- L[[1]]
    adapted.test.data <- L[[2]]

    # RF training step
    RF <- ranger::ranger(income ~ ., data = adapted.train.data, num.trees = 500,
      classification = T)
    Y.hat <- predict(RF, data = adapted.test.data)$predictions
  }

  return(c(sum(Y.hat == test.data[, label.col]) / nrow(test.data),
           abs(sum(as.integer(Y.hat[test.base.indicator])) / sum(test.base.indicator)  -
               sum(as.integer(Y.hat[!test.base.indicator])) / sum(!test.base.indicator)
              )
          )
        )
}

NRF <- ARF <- NULL
set.seed(12345)
nrep <- 10
subsample <- replicate(nrep, sample(1:nrow(data), round(floor(nrow(data)) * 3 / 4)))
for(i in 1:20) {
  train <-
  train.data <- data[train, ]
  test.data <- data[-train, ]

  NRF <- cbind(NRF, Acc_and_Gap(train.data, test.data, method = 1))
  print(NRF)

  ARF <- cbind(ARF, Acc_and_Gap(train.data, test.data, method = 2))
  print(ARF)
}

save.image(file = "adult.RData")
quit()





load("adult_ada.RData")
Y.true <- as.integer(data[-(1:train.size),1]) - 1

Y.probs.lfr <- adult_lfr_test <- readr::read_csv("~/AIF360/examples/adult_lfr_test.csv",
                                          col_names = FALSE)
Y.probs.lfr <- as.numeric(unlist(Y.probs.lfr))

summarise(Y.probs.adapted, Y.true, baseline.indicator)
summarise(Y.probs.hybrid, Y.true, baseline.indicator)
summarise(Y.probs.dropA, Y.true, baseline.indicator)
summarise(Y.probs.normal, Y.true, baseline.indicator)
summarise(Y.probs.lfr, Y.true, baseline.indicator)

gender.labels <- as.factor(as.integer(data$sex[1:18087]) - 1)
adapted.train.data <- cbind(adapted.train.data, gender.labels)

RF <- ranger::ranger(gender.labels ~ . - sex - income, data = adapted.train.data, num.trees = 500)
g.predict <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
g.probs <- sapply(1:nrow(adapted.test.data), function(x) mean(g.predict[x, ]) - 1)

#
preds <- g.probs
labels <- as.factor(as.integer(baseline.indicator))

# computing the lower-bound of the TV for the whole distribution P(.,.)
require(dWit)
twoSampleTvLb(labels = labels, preds = preds)

# computing the lower-bound of TV when only P(Y = 1 \mid X = x, A =a) is used
probs <- rbind(Y.probs.adapted, Y.probs.hybrid, Y.probs.dropA, Y.probs.normal, Y.probs.lfr)
for (i in 1:5) {
  prob <- probs[i,]
  fr <- as.data.frame(cbind(prob, labels))
  fr$labels <- as.factor(fr$labels - 1)
  ind.train <- sample(1:nrow(fr), size = nrow(fr)/2)
  # single tree
  rp <- rpart::rpart(labels~., data = fr[ind.train,], method = "class")
  preds <- predict(rp, newdata = fr[-ind.train,], type = "prob")[,"1"]
  print(twoSampleTvLb(labels = fr$labels[-ind.train], preds = preds))
}


# TV distance analysis - marginal differences
par(mfrow = c(2,3))
for (i in 4:8) {
  height <- rbind(table(adapted.train.data[gender.labels == 1, i]) / sum(gender.labels == 1),
                  table(adapted.train.data[gender.labels == 0, i]) / sum(gender.labels == 0))
  barplot(height, beside = TRUE)
}
