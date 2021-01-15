test_that("Fairadapt Works", {

  adjacency.matrix <- array(0, dim = c(3,3))
  colnames(adjacency.matrix) <- c("A","Y","X")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)

  adjacency.matrix["A", "X"] <- 1
  adjacency.matrix["X", "Y"] <- 1

  DataGen <- function(n) {

    A <- rbinom(n, size = 1, prob = 0.5)
    coeff <- 1 / 4
    dev <- 1
    X <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    Y <- rbinom(n, size = 1, prob = expit((X)))
    df <- data.frame(cbind(factor(Y), A, X))
    colnames(df) <- c("Y","A","X")

    df

  }

  cfd <- adjacency.matrix
  cfd[,] <- 0

  for(method in c("forest", "forest2", "linear")) {

    L <- fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
      adj.mat = adjacency.matrix, protect.A = "A", quant.method = method)

    expect_true(is.list(L))

    expect_true(is.data.frame(L[[1]]))

    expect_true(is.data.frame(L[[2]]))

  }

  data <- read.csv(file.path("..", "real-data", "compas", "compas-scores-two-years.csv"))
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

  train <- data[1:5000, ]
  test <- data[-(1:5000), ]

  compasL <- fairadapt(two_year_recid ~ ., train.data = train, test.data = test,
    adj.mat = adjacency.matrix, protect.A = "race", quant.method = method)

  expect_equal(compasL[[1]]$priors_count[train$race == "White"], train$priors_count[train$race == "White"])

  mse <- mean((compasL[[1]]$priors_count[!(train$race == "White")] - train$priors_count[!(train$race == "White")])^2)
  print(mse)
  expect_true(mse < 10)

})
