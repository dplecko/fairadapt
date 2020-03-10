library(fairadapt)
library(ggplot2)
root <- rprojroot::find_root(rprojroot::has_file("fairadapt.Rproj"))
# plotting function
getplots <- function(info, ResSet, label){
  vars <- c("auc", "tv", "cs")
  geom <- sapply(info, function(x) as.vector(sapply(1:3, function(i) c(mean(x[i, ]),
    mean(x[i, ]) - sd(x[i, ]),
    mean(x[i, ]) + sd(x[i, ])))))
  frame <- data.frame(t(geom))
  names(frame) <- as.vector(sapply(vars, function(x) paste0(x, 0:2)))
  override.shape <- c(4,8,15,16,17,18,21, 22)[1:nrow(frame)]
  vals <- c(15:20, 25, 24, 23)[1:nrow(frame)]
  shape <- (15:22)[1:nrow(frame)]
  g <- guide_legend("Resolving set / Method", ncol = 2L)
  ResSet <- c(as.character(ResSet), "reweighing", "reductions")
  p1 <- ggplot(data = frame, aes(x = tv0, y = auc0)) +
    geom_point(aes(shape = ResSet, color = ResSet), size = 5) +
    scale_shape_manual(values=vals) +
    guides(colour = g, shape = g) +
    geom_linerange(aes(ymin = auc1,ymax = auc2, color = ResSet)) +
    geom_errorbarh(aes(xmin = tv1,xmax = tv2, color = ResSet), height = 0) +
    xlab("Parity gap at 0.5 threshold") + ylab("Area under ROC") +
    ggtitle(paste0("Synthetic ", label, " - discrimination removal with resolvers"), subtitle = waiver()) +
    theme_bw() +
    theme(legend.position = c(0.8,0.275), legend.box.background = element_rect(color = "black"))

  p2 <- ggplot(data = frame, aes(x = tv0, y = cs0)) +
    geom_point(aes(shape = ResSet, color = ResSet), size = 5) +
    scale_shape_manual(values=vals) +
    guides(colour = g, shape = g) +
    geom_linerange(aes(ymin = cs1,ymax = cs2, color = ResSet)) +
    geom_errorbarh(aes(xmin = tv1,xmax = tv2, color = ResSet), height = 0)+
    xlab("Parity gap at 0.5 threshold") + ylab("Calibration score") +
    ggtitle(paste0("Synthetic ", label, " - discrimination removal with resolvers"), subtitle = waiver()) +
    theme_bw() +
    theme(legend.position = c(0.8,0.7),legend.box.background = element_rect(color = "black"))
  return(list(p1,p2))
}

# utility functions
CalibrationScore <- function(probs, labels, attr.label, k) {
  bins <- .bincode(probs, c(0:(k-1),k+1) / k , right = FALSE)
  c.diff <- sapply(1:k, function(x) {
                                    mean(labels[bins == x & attr.label == 0]) -
                                    mean(labels[bins == x & attr.label == 1])
                                    })
  return(sum(abs(c.diff)))
}
Measures <- function(S.hat, labels, attr.label, k) {
  auroc <- PRROC::roc.curve(scores.class0 = S.hat, weights.class0 = labels)$auc
  gap <- 100*abs(sum(S.hat[attr.label == 0] > 0.5) / sum(attr.label == 0) -
      sum(S.hat[attr.label == 1] > 0.5) / sum(attr.label == 1))
  cs <- CalibrationScore(probs = S.hat, labels = labels,
    attr.label = attr.label, k = 10)
  return(c(auroc, gap, cs))
}
GAUCS <- function(adj.mat, res.list, GenMech, prot.attr = "A", nrep = 5,
                   n.train = 2000, n.test = 2000) {
  res <- replicate(length(res.list), NULL)
  dat.train <- NULL
  dat.test <- NULL
  set.seed(12345)
  for(i in 1:nrep){
    train.data <- GenMech(n.train)
    test.data <- GenMech(n.test)
    test.base.ind <- test.data$A
    test.labels <- test.data$Y
    for(j in 1:length(res.list)){
      L <- fairadapt::fairadapt(Y ~ ., train.data = train.data,
        test.data = test.data, protect.A = prot.attr,
        res.vars = res.list[[j]], adj.mat = adjacency.matrix)
      adapted.train.data <- L[[1]]
      adapted.test.data <- L[[2]]
      RF <- ranger::ranger(Y ~ ., data = adapted.train.data, num.trees = 500,
                           classification = T)
      Y.hat <- predict(RF, data = adapted.test.data, predict.all = TRUE)$predictions
      S.hat <- sapply(1:nrow(Y.hat), function(i) mean(Y.hat[i, ]))
      res[[j]] <- cbind(res[[j]], Measures(S.hat = S.hat, labels = test.labels,
                                           attr.label = test.base.ind, k = k))
      print(sprintf("(replication = %d, resolving.case = %d) done", i, j))
    }
    dat.train[[i]] <- train.data
    dat.test[[i]] <- test.data
  }
  return(list(res, dat.train, dat.test))
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
  ResolvingLevelGenB <- function(n) {
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
nrep <- 5
result <- GAUCS(adj.mat = adjacency.matrix, res.list = resolvers,
                   GenMech = ResolvingLevelGen, nrep = nrep)
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

# plotting offline
{
  x <- getplots(info, ResSet, experiment)
  cowplot::plot_grid(x[[1]], x[[2]], nrow = 2L)
  ggsave(paste0(file.path(root, "..", "Article", paste0("synthetic", experiment)), ".png"),
    device = "png", width = 7, height = 7)
}
