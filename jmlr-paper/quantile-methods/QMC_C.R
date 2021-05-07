devtools::install(pkg = "..")
library(fairadapt)
library(ggplot2)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
source(file.path(root, "tests", "synthetic", "synthetic-helpers.R"))

experiment <- "C"

if (experiment == "A") {

  # Synthetic example 1

  adjacency.matrix <- array(0, dim = c(7,7))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3","X4","X5")
  resolve <- c("X1","X2","X3","X4","X5")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2", "X3", "X4", "X5")] <- 1
  adjacency.matrix[c("X1", "X2", "X3", "X4", "X5"), "Y"] <- 1

  ResolvingLevelGen <- function(n) {

    expit <- function(x) return(exp(x)/(1+exp(x)))

    A <- rbinom(n, size = 1, prob = 0.5)
    coeff <- 1 / 4
    dev <- 1
    X1 <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X2 <- -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X3 <- -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X4 <-  -A*coeff+ coeff/2 + rnorm(n, sd = dev)
    X5 <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    Y <- rbinom(n, size = 1, prob = expit((X1+X2+X3+X4+X5)))
    df <- data.frame(cbind(Y,A,X1,X2,X3,X4,X5))
    colnames(df) <- c("Y","A","X1","X2","X3","X4","X5")
    return(df)

  }

  resolvers <- list(NULL)

} else if (experiment == "B") {

  # Synthetic example 2

  adjacency.matrix <- array(0, dim = c(5, 5))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2")] <- 1
  adjacency.matrix[c("X1", "X2", "X3"), "Y"] <- 1
  adjacency.matrix["X2", "X3"] <- 1

  ResolvingLevelGen <- function(n) {

    expit <- function(x) return(exp(x)/(1+exp(x)))

    A <- rbinom(n, size = 1, prob = 0.5)
    coeff <- 1 / 4
    dev <- 1
    X1 <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X2 <- -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X3 <- 1/4 * X2 + rnorm(n, sd = dev)
    Y <- rbinom(n, size = 1, prob = expit((X1+X2+X3)))

    df <- data.frame(cbind(Y,A,X1,X2,X3))
    colnames(df) <- c("Y","A","X1","X2","X3")
    return(df)

  }

  resolvers <- list(NULL)

} else if (experiment == "C") {

  # Synthetic example 3

  adjacency.matrix <- array(0, dim = c(6, 6))
  colnames(adjacency.matrix) <- c("A", "Y", "X1", "X2", "X3", "X4")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2")] <- 1
  adjacency.matrix[c("X1", "X2"), "X3"] <- 1
  adjacency.matrix[c("X2", "X3"), "X4"] <- 1
  adjacency.matrix[c("X1", "X2", "X3", "X4"), "Y"] <- 1

  ResolvingLevelGen <- function(n) {

    expit <- function(x) return(exp(x)/(1+exp(x)))

    A <- rbinom(n, size = 1, prob = 0.5)
    X1 <-  runif(n) + (A == 0)*1/2
    X2 <-  runif(n) + (A == 0)*1/2
    X3 <-  X1*X2 + X1^2 + rnorm(n)
    X4 <- 1/4*X3^2 + 2*X2^2 + rnorm(n)
    Y <- rbinom(n, size = 1, prob = expit(1/2*(X1+X2+X3+X4-7)))

    df <- data.frame(cbind(Y, A, X1, X2, X3, X4))
    colnames(df) <- c("Y", "A", "X1", "X2", "X3", "X4")
    return(df)

  }

  resolvers <- list(NULL)

}

quant.methods <- c("forest", "linear", "nn")
result <- replicate(length(quant.methods), NULL)
nrep <- 10
n.sample <- 5000

for(i in 1:length(quant.methods)){

  set.seed(12345)

  result[[i]] <- GAUCS.parallel(adj.mat = adjacency.matrix, res.list = resolvers, quant.method = quant.methods[i],
    GenMech = ResolvingLevelGen, nrep = nrep, n.train = n.sample, n.test = n.sample)[[1]][[1]]

}

df <- Map(knitData, result, quant.methods)
df <- Reduce(rbind, df)

save(df, file = paste0("QMC_synthetic_", experiment, ".RData"))
