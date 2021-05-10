test_that("Fairadapt Class Works", {

  adjacency.matrix <- array(0, dim = c(4,4))
  colnames(adjacency.matrix) <- c("A", "Z", "Y","X")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)

  adjacency.matrix["A", "X"] <- 1
  adjacency.matrix["X", "Y"] <- 1
  adjacency.matrix["Z", c("Y", "X", "A")] <- 1

  DataGen <- function(n) {

    A <- rbinom(n, size = 1, prob = 0.5)
    Z <- rnorm(n)
    coeff <- 1 / 4
    dev <- 1
    X <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    Y <- rbinom(n, size = 1, prob = expit((X)))
    df <- data.frame(cbind(factor(Y), A, X, Z))
    colnames(df) <- c("Y","A","X", "Z")

    df

  }


  for(method in c(fairadapt:::rangerQuants,
                  fairadapt:::linearQuants)) {

      L <- fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
                     adj.mat = adjacency.matrix, protect.A = "A")

  }
  cts_data <- DataGen(100)
  cts_data$Y <- cts_data$X
  L_cts <- fairadapt(Y ~ ., train.data = cts_data, test.data = cts_data,
                 adj.mat = adjacency.matrix, protect.A = "A")

  # autoplot
  expect_true(ggplot2::is.ggplot(autoplot(L)))
  expect_true(ggplot2::is.ggplot(autoplot(L_cts)))

  # plot
  plot(L)
  plot(L_cts)
  plot(L, graph = TRUE)
  print(L)

  # fairTwins
  expect_true(is.data.frame(fairTwins(L)))
  expect_true(is.data.frame(fairTwins(L, train.id = NULL, test.id = 1L)))

  # predict function
  expect_true(is.data.frame(predict(L, DataGen(100))))

})
