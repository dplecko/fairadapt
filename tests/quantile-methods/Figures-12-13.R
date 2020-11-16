library(fairadapt)
library(ggplot2)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
source(file.path(root, "tests", "synthetic", "synthetic-helpers.R"))

experiment <- "C" # "B" for Figure 12, "C" for Figure 13

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

quant.methods <- c("forest", "linear", "neural network")
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

ggplot(data = df, aes(x = x, y = y)) +
    geom_point(aes(shape = quant.method, color = quant.method), size = 5) +
    scale_shape_manual(values=c(15:17)) +
    geom_linerange(aes(ymin = ymin,ymax = ymax, color = quant.method)) +
    geom_errorbarh(aes(xmin = xmin,xmax = xmax, color = quant.method), height = 0) +
    xlab("Parity gap at 0.5 threshold") + ylab("AUC") +
    ggtitle(paste0("Synthetic ", experiment, ": comparison of quantile methods"),
      subtitle = waiver()) +
    theme_bw() +
    guides(color=guide_legend(title="Quantile method"),
      shape=guide_legend(title="Quantile method")) +
    theme(legend.position = c(0.75,0.25),
      legend.box.background = element_rect(colour = "black"),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 16))

# save the plot
# ggsave(
#   paste0(file.path(root, "..", "..", "Article", "plots", paste0("QMC_", experiment)), ".png"),
#   device = "png", width = 7, height = 4
# )
