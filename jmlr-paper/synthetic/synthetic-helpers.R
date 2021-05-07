
# functionality for plotting
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
  p1 <- ggplot(data = frame, aes(x = tv0, y = auc0)) +
    geom_point(aes(shape = ResSet, color = ResSet), size = 5) +
    scale_shape_manual(values=vals) +
    guides(colour = g, shape = g) +
    geom_linerange(aes(ymin = auc1,ymax = auc2, color = ResSet)) +
    geom_errorbarh(aes(xmin = tv1,xmax = tv2, color = ResSet), height = 0) +
    xlab("Parity gap at 0.5 threshold") + ylab("Area under ROC") +
    ggtitle(paste0("Synthetic ", label, " - discrimination removal with resolvers"), subtitle = waiver()) +
    theme_bw() +
    theme(legend.position = c(0.8,0.28), legend.box.background = element_rect(color = "black"))

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

  c.diff <- sapply(sort(unique(bins)), function(x) {
                                    mean(labels[bins == x & attr.label == 0]) -
                                    mean(labels[bins == x & attr.label == 1])
                                    })
  return(sum(abs(c.diff)) * k / length(unique(bins)))

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
                   n.train = 2000, n.test = 2000, quant.method = "forest") {

  res <- replicate(length(res.list), NULL)

  dat.train <- lapply(1:nrep, function(i) GenMech(n.train))
  dat.test <- lapply(1:nrep, function(i) GenMech(n.test))

  for(i in 1:nrep){

    train.data <- dat.train[[i]]
    test.data <- dat.test[[i]]
    test.base.ind <- test.data$A
    test.labels <- test.data$Y

    for(j in 1:length(res.list)){

      L <- fairadapt::fairadapt(Y ~ ., train.data = train.data,
        test.data = test.data, protect.A = prot.attr,
        res.vars = res.list[[j]], adj.mat = adjacency.matrix, quant.method = quant.method)
      adapted.train.data <- L[[1]]
      adapted.test.data <- L[[2]]
      adapted.train.data <- adapted.train.data[, -which(names(adapted.train.data) == prot.attr)]
      log.fit <- glm(Y ~ ., data = adapted.train.data, family = "binomial")
      S.hat <- predict(log.fit, newdata = adapted.test.data, type = "response")

      res[[j]] <- cbind(res[[j]], Measures(S.hat = S.hat, labels = test.labels,
                                           attr.label = test.base.ind, k = k))
      print(sprintf("(replication = %d, resolving.case = %d) done", i, j))

    }

  }

  return(list(res, dat.train, dat.test))

}

GAUCS.parallel <- function(adj.mat, res.list, GenMech, prot.attr = "A", nrep = 5,
                   n.train = 2000, n.test = 2000, quant.method = "forest") {

  res <- replicate(length(res.list), NULL)

  dat.train <- lapply(1:nrep, function(i) GenMech(n.train))
  dat.test <- lapply(1:nrep, function(i) GenMech(n.test))

  dat <- Map(function(x, y) list(train = x, test = y), dat.train, dat.test)

  for(j in 1:length(res.list)){

    require(parallel)

    adapted.data <- mclapply(dat, function(x) {
      L <- fairadapt::fairadapt(Y ~ ., train.data = x[["train"]],
        test.data = x[["test"]], protect.A = prot.attr,
        res.vars = res.list[[j]], adj.mat = adjacency.matrix, quant.method = quant.method)
      names(L) <- c("train", "test")
      return(L)
    })

    for(i in 1:nrep){

      train.data <- dat.train[[i]]
      test.data <- dat.test[[i]]
      test.base.ind <- test.data$A
      test.labels <- test.data$Y

      adapted.train.data <- adapted.data[[i]][["train"]]
      adapted.test.data <- adapted.data[[i]][["test"]]
      adapted.train.data <- adapted.train.data[, -which(names(adapted.train.data) == prot.attr)]

      log.fit <- glm(Y ~ ., data = adapted.train.data, family = "binomial")
      S.hat <- predict(log.fit, newdata = adapted.test.data, type = "response")

      res[[j]] <- cbind(res[[j]], Measures(S.hat = S.hat, labels = test.labels,
                                           attr.label = test.base.ind, k = k))

    }

  }

  return(list(res, dat.train, dat.test))

}

knitData <- function(x, y) {

  r <- lapply(1:2, function(i) c(mean(x[i, ]), mean(x[i, ])+sd(x[i, ]), mean(x[i, ])-sd(x[i, ])))
  r <- Reduce(c, r)
  r <- data.frame(t(r))
  names(r) <- c("y", "ymax", "ymin", "x", "xmax", "xmin")
  r <- cbind(r, quant.method = y)

  return(r)

}
