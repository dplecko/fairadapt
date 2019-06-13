invisible(
  lapply("Example_Scripts.R", source)
  #lapply(list.files(file.path("./R"), full.names = TRUE), source)
)

# run UCI Adult
{
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

# UCI Adult data
data <- read.csv("UCIAdult.csv")
data <- data[, -1]
data[, "sex"] <- factor(data[, "sex"], levels = c("Male","Female"))
train.size <- 20600
test.size <- dim(data)[1] - train.size

L <- FairAdapt(income ~ ., train.data = data[1:train.size, ],
               test.data = data[train.size + 1:test.size, ], protected.attribute = "sex",
               adjacency.matrix = adjacency.matrix)
adapted.train.data <- L[[1]]
adapted.test.data <- L[[2]]
adapted.train.data[, "income"] <- data[1:train.size, "income"]

Y.hat <- ranger::csrf(income ~ ., training_data = adapted.train.data, test_data = adapted.test.data)
Y.hat <- as.integer(Y.hat) - 1
baseline.indicator <- data[train.size + 1:test.size, "sex"] == "Male"
Y <- as.integer(data[train.size + 1:test.size, "income"]) - 1

printf("Summary for the FairAdapt")
printf("")
FairnessMetrics(Y, Y.hat, baseline.indicator)
printf("")

printf("Summary for unconstrained")
printf("")
Y.hat.org <- ranger::csrf(income ~ ., training_data = data[1:train.size, ], test_data = data[train.size + 1:test.size, ])
Y.hat.org <- as.integer(Y.hat.org) - 1
FairnessMetrics(Y, Y.hat.org, baseline.indicator)
printf("")

printf("Summary for drop A")
printf("")
Y.hat.drop <- ranger::csrf(income ~ ., training_data = data[1:train.size, -2], test_data = data[train.size + 1:test.size, -2])
Y.hat.drop <- as.integer(Y.hat.drop) - 1
FairnessMetrics(Y, Y.hat.drop, baseline.indicator)
printf("")
}

# Run Equalised Odds example
{
  adjacency.matrix <- array(0, dim = c(5,5))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("Y", "X1", "X2", "X3")] <- 1
  adjacency.matrix["Y", c("X1", "X2", "X3")] <- 1

  n <- 10000
  data <- EqualisedOddsGenerator(n)
  train.size <- 7500
  test.size <- n - train.size

  L <- FairAdapt(Y ~ ., train.data = data[1:train.size, ],
                 test.data = data[train.size + 1:test.size, ], protected.attribute = "A",
                 resolving.variables = "Y", adjacency.matrix = adjacency.matrix)
  adapted.train.data <- L[[1]]
  adapted.test.data <- L[[2]]

  Y.hat <- ranger::csrf(income ~ ., training_data = adapted.train.data, test_data = adapted.test.data)
  Y.hat <- as.integer(Y.hat) - 1
  baseline.indicator <- data[train.size + 1:test.size, "A"] == 0
  Y <- as.integer(data[train.size + 1:test.size, "Y"]) - 1
  printf("Summary for the FairAdapt")
  printf("")
  FairnessMetrics(Y, Y.hat, baseline.indicator)
  printf("")
}

# Run Demographic parity example
{
  adjacency.matrix <- array(0, dim = c(4,4))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2")] <- 1
  adjacency.matrix[c("X1", "X2"), "Y"] <- 1

  n <- 10000
  data <- DemParityGenerator(n)
  data[, 1] <- as.factor(data[, 1])
  data[, 2] <- as.factor(data[, 2])


  train.size <- 7500
  test.size <- n - train.size

  L <- FairAdapt(Y ~ ., train.data = data[1:train.size, ],
                 test.data = data[train.size + 1:test.size, ], protected.attribute = "A",
                 adjacency.matrix = adjacency.matrix, resolving.variables = "X1")
  adapted.train.data <- L[[1]]
  adapted.test.data <- L[[2]]
  #adapted.train.data[, "Y"] <- data[1:train.size, "Y"]

  #plot the difference afterwards (can't be done since the indicator is lost)
  #plot(sort(adapted.train.data[data[,2]==0 & (1:5000) <= 3500,4]))
  #points(sort(adapted.train.data[data[,2]==1 & (1:5000) <= 3500,4]), col = "red")

  adapted.forest <- ranger::ranger(Y ~ ., data = adapted.train.data)
  adapted.predict <- predict(adapted.forest, data = adapted.test.data, predict.all = TRUE)$predictions
  prediction.probs.adapt <- sapply(1:test.size, function(x) mean(adapted.predict[x, ]) - 1)
  baseline.indicator <- data[train.size + 1:test.size, "A"] == 0
  Y <- as.integer(data[train.size + 1:test.size, "Y"]) - 1
  Y.hat <- round(prediction.probs.adapt)

  original.forest <- ranger::ranger(Y ~ ., data = data[1:train.size, ])
  Y.hat.org <- predict(original.forest, data = data[train.size + 1:test.size, ])$predictions
  Y.hat.org <- as.integer(Y.hat.org) - 1

  hybrid.predict <- predict(original.forest, data = adapted.test.data, predict.all = TRUE)$predictions
  prediction.probs.hybrid <- sapply(1:test.size, function(x) mean(hybrid.predict[x, ]) - 1)
  Y.hat.hybrid <- round(prediction.probs.hybrid)

  plot(prediction.probs.hybrid, prediction.probs.adapt,
       col = as.integer(baseline.indicator)+1, pch = 19,
       xlim = c(0,1), ylim = c(0,1))
  abline(0,1)

  printf("Summary for original")
  print(FairnessMetrics(Y, Y.hat.org, baseline.indicator))
  printf("All Adapted")
  print(FairnessMetrics(Y, Y.hat, baseline.indicator))
  printf("Summary for hybrid")
  print(FairnessMetrics(Y, Y.hat.hybrid, baseline.indicator))
}

# Run Resolving example
{
  adjacency.matrix <- array(0, dim = c(7,7))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3","X4","X5")
  resolve <- c("X1","X2","X3","X4","X5")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2", "X3", "X4", "X5")] <- 1
  adjacency.matrix[c("X1", "X2", "X3", "X4", "X5"), "Y"] <- 1

  n <- 5000
  train.size <- 3500
  test.size <- n - train.size
  acc <- array(0, dim = c(6,2))
  acc.dev <- array(0, dim = c(6,2))
  gap <- array(0, dim = c(6,2))
  gap.dev <- array(0, dim = c(6,2))

  for (i in 1:6) {
    adapted <- NULL
    hybrid <- NULL
    if ( i == 1 ){
      resolving.variables <- NULL
    }
    else {
      resolving.variables <- resolve[1:(i-1)]
    }
    for (j in 1:5) {
      data <- ResolvingLevelGen(n)
      data[, 1] <- as.factor(data[, 1])
      data[, 2] <- as.factor(data[, 2])
      L <- FairAdapt(Y ~ ., train.data = data[1:train.size, ],
                     test.data = data[train.size + 1:test.size, ], protected.attribute = "A",
                     adjacency.matrix = adjacency.matrix, resolving.variables = resolving.variables)
      adapted.train.data <- L[[1]]
      adapted.test.data <- L[[2]]

      adapted.forest <- ranger::ranger(Y ~ ., data = adapted.train.data)
      Y.hat <- predict(adapted.forest, data = adapted.test.data)$predictions
      baseline.indicator <- data[train.size + 1:test.size, "A"] == 0
      Y <- as.integer(data[train.size + 1:test.size, "Y"]) - 1
      Y.hat <- as.integer(Y.hat) - 1

      original.forest <- ranger::ranger(Y ~ ., data = data[1:train.size, ])
      Y.hat.hybrid <- predict(original.forest, data = adapted.test.data)$predictions
      Y.hat.hybrid <- as.integer(Y.hat.hybrid) - 1

      adapted <- rbind(adapted, FairnessMetrics(Y, Y.hat, baseline.indicator))
      hybrid <- rbind(hybrid, FairnessMetrics(Y, Y.hat.hybrid, baseline.indicator))
    }
    acc[i, 1] <- mean(adapted[, 1])
    acc[i, 2] <- mean(hybrid[, 1])
    acc.dev[i, 1] <- sd(adapted[, 1])
    acc.dev[i, 2] <- sd(hybrid[, 1])

    gap[i, 1] <- mean(adapted[, 2])
    gap[i, 2] <- mean(hybrid[, 2])
    gap.dev[i, 1] <- sd(adapted[, 2])
    gap.dev[i, 2] <- sd(hybrid[, 2])
  }

}

# Parity-Calibration interpolation
{
  adjacency.matrix <- array(0, dim = c(7,7))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3","X4","X5")
  resolve <- c("X1","X2","X3","X4","X5")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2", "X3", "X4", "X5")] <- 1
  adjacency.matrix[c("X1", "X2", "X3", "X4", "X5"), "Y"] <- 1

  n <- 5000
  train.size <- 3500
  test.size <- n - train.size

  data <- ResolvingLevelGen(n)
  data[, 1] <- as.factor(data[, 1])
  data[, 2] <- as.factor(data[, 2])
  L <- FairAdapt(Y ~ ., train.data = data[1:train.size, ],
                 test.data = data[train.size + 1:test.size, ], protected.attribute = "A",
                 adjacency.matrix = adjacency.matrix, resolving.variables = NULL)
  adapted.train.data <- L[[1]]
  adapted.test.data <- L[[2]]

  # train the hybrid forest and get probabilities
  original.forest <- ranger::ranger(Y ~ ., data = data[1:train.size, ])
  Y.hat <- predict(adapted.forest, data = adapted.test.data, predict.all = TRUE)$predictions
  Y.hat.probs <- sapply(1:test.size, function(x) mean(Y.hat[x, ]) - 1)
  baseline.indicator <- data[train.size + 1:test.size, "A"] == 0
  Y <- as.integer(data[train.size + 1:test.size, "Y"]) - 1

  # compute the original probabilities
  Y.org <- predict(original.forest, data = data[train.size + 1:test.size, ], predict.all = TRUE)$predictions
  Y.org.probs <- sapply(1:test.size, function(x) mean(Y.org[x, ]) - 1)

  # do binning - create lists of indicators
  L.hybrid <- L.org <- list(NULL)
  for (i in 1:10) {
    L.hybrid[[i]] <- (Y.hat.probs >= (i-1)/10) & (Y.hat.probs <= i/10)
    L.org[[i]] <- (Y.org.probs >= (i-1)/10) & (Y.org.probs <= i/10)
  }
  m.adapt <- sapply(1:10, function(x) mean(Y[L.hybrid[[x]] & baseline.indicator]))
  f.adapt <- sapply(1:10, function(x) mean(Y[L.hybrid[[x]] & !baseline.indicator]))
  m.org <- sapply(1:10, function(x) mean(Y[L.org[[x]] & baseline.indicator]))
  f.org <- sapply(1:10, function(x) mean(Y[L.org[[x]] & !baseline.indicator]))

  grid <- 1:10 / 10
  par(mfrow = c(1, 2))
  plot(grid, m.adapt, pch = 19, xlim = c(0,1), ylim = c(0,1))
  points(grid, f.adapt, col = "red", pch = 19)
  abline(0,1)

  plot(grid, m.org, pch = 19, xlim = c(0,1), ylim = c(0,1))
  points(grid, f.org, col = "red", pch = 19)
  abline(0,1)
}

# write out a csv for DEM.PAR example
{
  # change the labels
  data.new <- RecodeLevels(data)
  write.csv(data.new[1:train.size, ], file = "~/AIF360/aif360/data/raw/custom/custom.data",
            row.names = FALSE)
  write.csv(data.new[1:test.size + train.size, ], file = "~/AIF360/aif360/data/raw/custom/custom.test",
            row.names = FALSE)
}
