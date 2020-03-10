library(fairadapt)
library(ggplot2)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))

auc_gap <- function(S.hat, labels, attr.label) {
  auroc <- PRROC::roc.curve(scores.class0 = S.hat, weights.class0 = labels)$auc
  gap <- 100*abs(sum(S.hat[attr.label == 0] > 0.5) / sum(attr.label == 0) -
      sum(S.hat[attr.label == 1] > 0.5) / sum(attr.label == 1))
  return(c(auroc, gap))
}
imbalance_effect <- function(adj.mat, probz, GenMech, prot.attr = "A", nrep = 5,
                   n.train = 5000, n.test = 5000) {
  dat.train <- NULL
  dat.test <- NULL
  set.seed(12345)
  df <- NULL
  for(i in 1:length(probz)){
    res <- list(A = NULL, B = NULL)
    for(j in 1:nrep){
      train.data <- GenMech(n.train, probz[i])
      test.data <- GenMech(n.test, probz[i])
      test.base.ind <- test.data$A
      test.labels <- test.data$Y

      L <- fairadapt::fairadapt(Y ~ ., train.data = train.data,
        test.data = test.data, protect.A = prot.attr,
        res.vars = NULL, adj.mat = adjacency.matrix)
      adapted.train.data <- L[[1]]
      adapted.test.data <- L[[2]]
      for (option in c("A", "B")) {
        if (option == "A") {
          RF <- ranger::ranger(Y ~ ., data = train.data, num.trees = 500,
                               classification = T)
        } else {
          RF <- ranger::ranger(Y ~ ., data = adapted.train.data, num.trees = 500,
                               classification = T)
        }
        Y.hat <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
        S.hat <- sapply(1:nrow(Y.hat), function(k) mean(Y.hat[k, ]))
        res[[option]] <- rbind(res[[option]], auc_gap(S.hat = S.hat, labels = test.labels,
                                    attr.label = test.base.ind))
      }
      print(sprintf("(prob = %.2f, repetition = %d) done", probz[i], j))
    }
    add.bottom <- lapply(res, function(x) {
      intm <- lapply(1:2, function(k) data.frame(cbind(mean = mean(x[, k]), sd = sd(x[, k]), prob = probz[i])))
      intm <- Reduce(rbind, intm)
      return(cbind(intm, val = c("auc", "gap")))
    })
    add.bottom <- Map(function(x, y) cbind(x, method = y), add.bottom, c("A", "B"))
    add.bottom <- Reduce(rbind, add.bottom)
    df <- rbind(df, add.bottom)
  }
  names(df) <- c("mean", "sd", "prob", "val", "method")
  return(df)
}

experiment <- "A"

if (experiment == "A") {
  # Synthetic example 1
  adjacency.matrix <- array(0, dim = c(7,7))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3","X4","X5")
  resolve <- c("X1","X2","X3","X4","X5")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2", "X3", "X4", "X5")] <- 1
  adjacency.matrix[c("X1", "X2", "X3", "X4", "X5"), "Y"] <- 1
  ResolvingLevelGenImbalanced <- function(n, prob) {
    expit <- function(x) return(exp(x)/(1+exp(x)))
    A <- rbinom(n, size = 1, prob = prob)
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
  resolvers <- c(list(NULL), lapply(1:5, function(i) paste0("X",1:i)))
  ResSet <- factor(c("null", "(X1)", "(X1, X2)", "(X1, X2, X3)",
                      "(X1, X2, X3, X4)", "(X1, X2, X3, X4, X5)"))
} else if (experiment == "B") {
  # Synthetic example 2
  adjacency.matrix <- array(0, dim = c(5,5))
  colnames(adjacency.matrix) <- c("A","Y","X1","X2","X3")
  rownames(adjacency.matrix) <- colnames(adjacency.matrix)
  adjacency.matrix["A", c("X1", "X2")] <- 1
  adjacency.matrix[c("X1", "X2", "X3"), "Y"] <- 1
  adjacency.matrix["X2", "X3"] <- 1
  ResolvingLevelGenBImbalanced <- function(n, prob) {
    expit <- function(x) return(exp(x)/(1+exp(x)))
    A <- rbinom(n, size = 1, prob = prob)
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
result <- imbalance_effect(adj.mat = adjacency.matrix, probz = seq(0.1, 0.9, 0.1),
                   GenMech = ResolvingLevelGenImbalanced, nrep = nrep)

p1 <- ggplot(result[result$val == "auc", ], aes(prob, mean, color = method, fill = method)) +
  geom_line(size = 1.5) + geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.4, linetype = 0) +
  xlab("Proportion of A = 0") + ylab("AUC") + theme_bw(15) +
    theme(legend.position="bottom") +
    scale_colour_brewer(type = "qual", palette = 6, direction = 1)

p2 <- ggplot(result[result$val == "gap", ], aes(prob, mean, color = method, fill = method)) +
  geom_line(size = 1.5) + geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.4, linetype = 0) +
  xlab("Proportion of A = 0") + ylab("Parity gap at 0.5 threshold") + theme_bw(15) + theme(legend.position="bottom") +
  scale_colour_brewer(type = "qual", palette = 6, direction = 1)

cowplot::plot_grid(p2, p1, nrow = 2L)
