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

  for(method in c(fairadapt:::rangerQuants,
                  fairadapt:::linearQuants,
                  fairadapt:::mcqrnnQuants)) {

    L <- fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
      adj.mat = adjacency.matrix, protect.A = "A", quant.method = method)

    expect_true(is.list(L))

    expect_true(is.data.frame(L[[1]]))

    expect_true(is.data.frame(L[[2]]))

  }

  L <- fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
                 top.ord = c("A", "X", "Y"), protect.A = "A")
  expect_true(is.list(L))

  skip_on_cran()

  data <- system.file("testdata", "compas-scores-two-years.rds",
                      package = "fairadapt")
  data <- readRDS(data)

  columns.keep <- which(names(data)
                        %in% c("age", "sex", "juv_fel_count",
                               "juv_misd_count", "juv_other_count",
                               "priors_count","c_charge_degree", "race",
                               "two_year_recid")
  )

  data <- data[, columns.keep]
  levels(data$race) <- c("Non-White", "Non-White", "White", "Non-White",
                         "Non-White", "Non-White")
  data$race <- relevel(data$race, "White")

  adjacency.matrix <- array(0, dim = c(ncol(data), ncol(data)))
  colnames(adjacency.matrix) <- c("age", "sex", "juv_fel_count",
                                  "juv_misd_count", "juv_other_count",
                                  "priors_count",
                                  "c_charge_degree", "race", "two_year_recid")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)

  # adding the edges to the matrix
  adjacency.matrix[c("race", "sex", "age"),
                   c("juv_fel_count", "juv_misd_count",
                     "juv_other_count", "priors_count",
                     "c_charge_degree", "two_year_recid")] <- 1
  adjacency.matrix[c("juv_fel_count", "juv_misd_count", "juv_other_count"),
                   c("priors_count", "c_charge_degree", "two_year_recid")] <- 1
  adjacency.matrix["priors_count", c("c_charge_degree", "two_year_recid")] <- 1
  adjacency.matrix["c_charge_degree", "two_year_recid"] <- 1

  train <- data[1:1000, ]
  test <- data[1001:2000, ]

  compasL <- fairadapt(two_year_recid ~ ., train.data = train, test.data = test,
                       adj.mat = adjacency.matrix, protect.A = "race",
                       quant.method = fairadapt:::rangerQuants)

  expect_equal(compasL$adapt.train$priors_count[train$race == "White"],
               train$priors_count[train$race == "White"])

  mse <- mean((compasL[[1]]$priors_count[!(train$race == "White")] -
                 train$priors_count[!(train$race == "White")])^2)

  expect_true(mse < 10)


  pred_a123 <- function(adj.mat, cov.mat, phi, n = 5000, res = NULL, CFD = T,
                        ...) {

    c_SEM <- function(A, eps, res) {

      L <- list()
      X <- list()

      for (i in 1:2) {

        for(j in 1:ncol(eps)) {

          if(!(paste0("X", j) %in% res) | i == 1)
            X[[j]] <- phi[[j]](A[[i]], X, eps[, j])

        }


        df <- data.frame(A = A[[i]], Reduce(cbind, X))
        names(df) <- c("A", paste0("X", 1:ncol(eps)))
        L[[i]] <- df

      }

      L

    }

    cfd.mat <- ( (cov.mat != 0) + diag(ncol(cov.mat)) ) * CFD

    set.seed(20211)
    eps <- 1 * mvtnorm::rmvnorm(n, mean = rep(0, length(phi)),
                                sigma = cov.mat[-1, -1] + diag(length(phi)))
    A <- rbinom(n, 1, 0.5)

    train.idx <- seq_len(n / 2)
    train <- c_SEM(list(A, 0), eps, res)

    test <- train[[1]][-train.idx, ]
    cf <- train[[2]][-train.idx, ]
    train <- train[[1]][train.idx, ]


    # do adaptation
    fair.sep <-
      fairadapt::fairadapt(as.formula(paste0("X", length(phi), " ~ .")),
                           train.data = train, test.data = NULL,
                           adj.mat = adj.mat, cfd.mat = cfd.mat,
                           protect.A = "A", res.vars = res,
                           ...)

    fair.sep <- predict(fair.sep, newdata = test)

    fair.join <-
      fairadapt::fairadapt(as.formula(paste0("X", length(phi), " ~ .")),
                           train.data = train, test,
                           adj.mat = adj.mat, cfd.mat = cfd.mat,
                           protect.A = "A", res.vars = res,
                           ...)[["adapt.test"]]



    par(mfrow = c(length(phi) - 1L, 2L))

    idx <- test$A == 1

    for(i in paste0("X", seq_len(length(phi) - 1L))) {

      MSE1 <- 100 * mean((fair.join[idx, i] - cf[idx, i])^2)
      MSE2 <- 100 * mean((fair.sep[idx, i] - cf[idx, i])^2)

      if (i == "X1") expect_true((MSE1 < 5) & (MSE2 < 5))
      if (i == "X2") expect_true((MSE1 < 10) & (MSE2 < 10))
      if (i == "X3") expect_true((MSE1 < 100) & (MSE2 < 100))

    }

  }

  f <- list(
    function(A, X, eps1) 2*A - 1 + eps1,
    function(A, X, eps2) 2*A - 1 + 1/25 * (X[[1]]+5)^2 + eps2,
    function(A, X, eps3) 1/4 * (X[[1]]+5) * (X[[2]]+6) + eps3,
    function(A, X, eps4) 0 * A + 0 / 2 * X[[1]] + 1 * X[[2]]*log((X[[2]])+50) +
      1 / 50 * X[[3]]^3 + eps4
  )

  adj.mat <- array(0, dim = c(length(f) + 1, length(f) + 1))
  colnames(adj.mat) <- rownames(adj.mat) <- c("A", paste0("X", 1:length(f)))
  adj.mat["A", "X1"] <-
    adj.mat["A", "X2"] <-
    adj.mat["X1", "X2"] <-
    adj.mat["X1", "X3"] <-
    adj.mat["X2", "X3"] <-
    adj.mat["X3", "X4"] <-
    adj.mat["X2", "X4"] <-
    1

  cov.mat <- adj.mat
  cov.mat[, ] <- 0

  pred_a123(adj.mat, cov.mat, phi = f, CFD = T, n=2000,
            quant.method = fairadapt:::rangerQuants)

  pred_a123(adj.mat, cov.mat, phi = f, CFD = T, n=2000,
            quant.method = fairadapt:::linearQuants)



})
