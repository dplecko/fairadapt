library(fairadapt)
library(ggplot2)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
source(file.path(root, "tests", "synthetic", "synthetic-helpers.R"))

experiment <- "A" # "A" for Figures 2 & 17, "B" for Figures 3 & 18

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
    sigma <- rnorm(n, sd = 0.5)
    X1 <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X2 <- -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X3 <- -A*coeff + coeff/2 + rnorm(n, sd = dev)
    X4 <-  -A*coeff+ coeff/2 + rnorm(n, sd = dev)
    X5 <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
    Y <- rbinom(n, size = 1, prob = expit((X1+X2+X3+X4+X5+sigma)))

    df <- data.frame(cbind(Y,A,X1,X2,X3,X4,X5))
    colnames(df) <- c("Y","A","X1","X2","X3","X4","X5")

    return(df)

  }

  resolvers <- c(list(NULL), lapply(1:5, function(i) paste0("X",1:i)))
  ResSet <- factor(c("null", "(X1)", "(X1, X2)", "(X1, X2, X3)",
                      "(X1, X2, X3, X4)", "(X1, X2, X3, X4, X5)"))

} else if (experiment == "B") {

  # Synthetic example 2

  adjacency.matrix <- array(0, dim = c(5,5))
  colnames(adjacency.matrix) <- c("A", "Y", "X1", "X2", "X3")
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

  resolvers <- list(r1 = NULL, r2 = "X1", r3 = "X2", r4 = "X3", r5 = c("X2", "X3"),
                    r6 = c("X1", "X2"), r7 = c("X1", "X3"),
                    r8 = c("X1", "X2", "X3"))
  ResSet <- as.factor(c("null", "(X1)", "(X2)", "(X3)", "(X2, X3)", "(X1, X2)",
                         "(X1, X3)", "(X1, X2, X3)"))

}

nrep <- 10
n.sample <- 5000

result <- GAUCS(adj.mat = adjacency.matrix, res.list = resolvers,
                   GenMech = ResolvingLevelGen, nrep = nrep, n.train = n.sample, n.test = n.sample)

info <- result[[1]]
train <- result[[2]]
test <- result[[3]]

# AIF360 comparisons
{
reticulate::use_python("/anaconda3/bin/python3.7")
library(reticulate)
py_run_string("from importlib import reload")
source_python(file.path(root, "tests", "synthetic", "reweighing_synthetic.py"))
source_python(file.path(root, "tests", "synthetic", "reductions_synthetic.py"))

add.comparisons <- list(NULL, NULL)
# compute the performance of reweighing and reductions
for(i in 1:nrep){

  print(i)
  py_run_string("moments = reload(moments)")
  py_run_string("red = reload(red)")

  S.hat <- reweigh_and_predict(r_to_py(train[[i]]), r_to_py(test[[i]]))
  add.comparisons[[1]] <- cbind(add.comparisons[[1]], Measures(S.hat, test[[i]]$Y, test[[i]]$A, k = 10))

  S.hat <- reduce_and_predict(r_to_py(train[[i]]), r_to_py(test[[i]]), 0.1)
  add.comparisons[[2]] <- cbind(add.comparisons[[2]], Measures(S.hat, test[[i]]$Y, test[[i]]$A, k = 10))

}

# assign the results to infoA, infoB
info <- c(info, add.comparisons)
}

# Figure 2/3
{
  x <- getplots(info[-8], c(as.character(ResSet),"reweighing", "reductions")[-8], experiment)
  cowplot::plot_grid(x[[1]], x[[2]], nrow = 2L)
}

# Figure 17/18
{
  x <- getplots(info, c(as.character(ResSet),"reweighing", "reductions"), experiment)
  cowplot::plot_grid(x[[1]], x[[2]], nrow = 2L)
}

# save the plots
#ggsave(paste0(file.path(root, "..", "..", "Article", paste0("synthetic", experiment)), ".png"),
#  device = "png", width = 7, height = 7)
