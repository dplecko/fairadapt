test_that("Input Verification Works", {
  
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
  
  L <- ReorderCols(Y ~ ., DataGen(100), DataGen(100), "A")
  
  expect_true(all(is.na(L[["test.data"]]$Y)))
  expect_equal(names(L[["train.data"]]), c("Y", "A", "X"))
  expect_equal(nrow(L[["train.data"]]), 100L)
  expect_equal(nrow(L[["test.data"]]), 100L)
  
  L2 <- InitAdapt(L[["org.data"]], "A")
  expect_true(is.factor(L2[["adapt.data"]]$A))
  expect_true(is.logical(L2[["base.ind"]]))
  
  expect_error(fairadapt(Y ~ Z + X, train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix+1, protect.A = "A"))
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A", quant.method = "noname"))
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A", res.vars = "noname"))
  
  na.dat <- DataGen(100)
  na.dat$X <- NA
  
  expect_error(fairadapt(Y ~ ., train.data = na.dat, test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = na.dat,
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  w.dat <- cbind(DataGen, Z = 0)
  
  expect_error(fairadapt(Y ~ ., train.data = w.dat, test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  adjacency.matrix["Y", "X"] <- 1L
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  rownames(adjacency.matrix) <- colnames(adjacency.matrix) <- NULL
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  rownames(adjacency.matrix) <- colnames(adjacency.matrix) <- c("A", "A", "A")
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  rownames(adjacency.matrix) <- c("A", "X", "Y")
  colnames(adjacency.matrix) <- c("A", "X2", "Y")
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  colnames(adjacency.matrix) <- c("A", "X1", "Y")
  rownames(adjacency.matrix) <- c("A", "A", "Y")
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix, protect.A = "A"))
  
  colnames(adjacency.matrix) <- c("A", "X1", "Y")
  rownames(adjacency.matrix) <- c("A", "X1", "Y")
  
  expect_error(fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
    adj.mat = adjacency.matrix[c("X1", "Y"), c("X1", "Y")], protect.A = "A"))
  
})