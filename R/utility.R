
CategoricalEncoding <- function(outcome, var, u.val) {

  cond.expect <- cat.enc <- rep(0, length(u.val))

  for (i in 1:length(u.val)) {

    cond.expect[i] <- mean(as.numeric(outcome[var == u.val[i]]), na.rm = TRUE)

    assertthat::assert_that(!is.na(cond.expect[i]),
      msg = "New factor levels appearing in the test data, which is disallowed")

  }

  if (length(cond.expect) != length(unique(cond.expect)))
    cond.expect <- cond.expect +
      rnorm(length(cond.expect), sd = sd(cond.expect)/10000)

  for(i in 1:length(u.val)) {

    cat.enc[i] <- sum(cond.expect <= cond.expect[i])

  }

  cat.enc

}

MarginalMatching <- function(x, base.ind) {

  x.baseline <- round(x[base.ind])
  x.non.baseline <- x[!base.ind]

  fitted.value.counts <- (tabulate(x.baseline) / length(x.baseline)) *
                          length(x.non.baseline)
  fitted.x.baseline <- NULL

  for (i in 1:length(fitted.value.counts)) {

    fitted.x.baseline <- c(fitted.x.baseline,
                           rep(i, round(fitted.value.counts[i])))

  }

  fitted.x.baseline <- c(fitted.x.baseline,
                         rep(length(fitted.value.counts),
                             max(0, length(x.non.baseline) - length(fitted.x.baseline))))
  fitted.x.baseline <- fitted.x.baseline[1:length(x.non.baseline)]

  x.non.baseline[order(x.non.baseline)] <- fitted.x.baseline
  x[!base.ind] <- x.non.baseline
  x[base.ind] <- x.baseline

  x

}

ReorderCols <- function(formula, train.data, test.data, protect.A) {

  train.data <- model.frame(formula, train.data)
  test.data <- test.data[, colnames(train.data)[-1]]

  train.data[, protect.A] <- as.factor(train.data[, protect.A])
  test.data[, protect.A] <- as.factor(test.data[, protect.A])
  test.data <- cbind(NA, test.data)

  colnames(test.data) <- colnames(train.data)

  list(
    train.data = train.data,
    test.data = test.data,
    org.data = rbind(train.data, test.data),
    train.len = nrow(train.data),
    full.len = nrow(train.data) + nrow(test.data)
  )

}

InitAdapt <- function(org.data, protect.A) {

  adapt.data <- org.data
  base.lvl <- levels(org.data[, protect.A])[1]
  base.ind <- org.data[, protect.A] == base.lvl
  adapt.data[, protect.A] <- factor(base.lvl,
                                    levels = levels(org.data[, protect.A]))

  list(
    adapt.data = adapt.data,
    base.ind = base.ind
  )

}

CtfAAP <- function(data, cf.parents, ind, A.root, quant.method = "forest") {

  nosplit <- (!A.root) | (quant.method == "nn")

  assertthat::assert_that(ncol(data) == (ncol(cf.parents)+1))

  names(data) <- c("Y", paste0("X", 1:ncol(cf.parents)))
  names(cf.parents) <- paste0("X", 1:ncol(cf.parents))

  if (nosplit) {

    U <- GetQuants(data, quant.method)
    ctf.values <- InvertQ(data, cf.parents, U, U[!ind], quant.method)

  } else {

    U <- rep(0, nrow(data))
    U[ind] <- GetQuants(data[ind, ], quant.method)
    U[!ind] <- GetQuants(data[!ind, ], quant.method)
    ctf.values <- InvertQ(data[ind, ], cf.parents, U[ind], U[!ind], quant.method)

  }

  ctf.values

}

GetQuants <- function(data, quant.method) {

  if(quant.method == "forest") {

    object <- ranger::ranger(formula(data), data = data, quantreg = T,
                             keep.inbag = T, min.node.size = 20)

    empirical <- object$random.node.values.oob

  } else if (quant.method == "nn") {

    data.matrix <- matrix(as.numeric(unlist(data)), nrow=nrow(data))
    object <- qrnn::mcqrnn.fit(x = data.matrix[, -1, drop = FALSE],
                               y = matrix(data.matrix[, 1], ncol = 1),
                               tau = seq(0.005, 0.995, by = 0.01),
                               n.trials = 1, iter.max = 500, trace = FALSE)

    x <- matrix(as.numeric(unlist(data[, -1, drop = FALSE])), nrow=nrow(data))

    empirical <- qrnn::mcqrnn.predict(x = x, parms = object)

  } else if (quant.method == "linear") {

    offending.cols <- 1 + which(sapply(2:ncol(data),
                                  function(x) length(unique(data[, x]))) == 1)
    keep.cols <- which(!(1:ncol(data) %in% offending.cols))

    if (length(offending.cols) == (ncol(data)-1)) {

      object <- quantreg::rq(Y ~ 1, data = data,
                             tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    } else {

      object <- quantreg::rq(formula(data[, keep.cols]), data = data,
                             tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    }

    empirical <- predict(object, newdata = data[, -1, drop = FALSE])

  }

  eval <- data[, 1]
  U.hat <- sapply(1:nrow(data), function(x) ecdf(empirical[x, ]) (eval[x]))

  return(U.hat)

}

InvertQ <- function(data, newdata, U, newU, quant.method) {

  if (quant.method == "forest") {

    object <- ranger::ranger(formula(data), data = data, quantreg = T,
                             min.node.size = 20)

    quantiles <- predict(object, data = newdata, type = "quantiles",
                         what = function(x) x)$predictions

  } else if (quant.method == "nn") {

    data.matrix <- matrix(as.numeric(unlist(data)), nrow=nrow(data))

    object <- qrnn::mcqrnn.fit(x = data.matrix[, -1, drop = FALSE],
                               y = matrix(data.matrix[, 1], ncol = 1),
                               tau = seq(0.005, 0.995, by = 0.01),
                               n.trials = 1, iter.max = 500, trace = FALSE)

    x <- matrix(as.numeric(unlist(newdata)), ncol = ncol(newdata))

    quantiles <- qrnn::mcqrnn.predict(x = x, parms = object)

  } else if (quant.method == "linear") {

    offending.cols <- 1 + which(sapply(2:ncol(data),
                                  function(x) length(unique(data[, x]))) == 1)
    keep.cols <- which(!(1:ncol(data) %in% offending.cols))

    if (length(offending.cols) == (ncol(data)-1)) {

      object <- quantreg::rq(Y ~ 1, data = data,
                             tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    } else {

      object <- quantreg::rq(formula(data[, keep.cols]), data = data,
        tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    }

    quantiles <- predict(object, newdata = newdata)

  }

  if(!is.null(quantiles)) ctf.values <- sapply(1:nrow(newdata),
    function(x) quantile(quantiles[x, ], newU[x]))

  ctf.values

}

EncodeDiscrete <- function(outcome, var) {

  assertthat::assert_that(length(var) == length(outcome))

  if (is.factor(var)) {

    u.val <- sort(unique(as.character(var)))
    cat.enc <- CategoricalEncoding(outcome, var, u.val)

  } else {

    u.val <- sort(unique(var))
    cat.enc <- order(order(u.val))

  }

  int.enc <- rep(NA, length(var))

  for (i in 1:length(u.val)) {

    int.enc[(var == u.val[i])] <- cat.enc[i]

  }

  int.enc <- int.enc + runif(length(var), -0.5, 0.5)

  list(
    discrete = TRUE,
    cat.enc = cat.enc,
    int.enc = int.enc,
    unique.values = u.val
  )

}

DecodeDiscrete <- function(var, cat.enc, u.val) {

  var <- round(var)
  int.decode <- rep(NA, length(var))

  for (i in 1:length(u.val)) {

      int.decode[(var == cat.enc[i])] <- u.val[i]

  }

  int.decode

}

Quantiles <- function(data, cat.par) {

  colnames(data)[1] <- "X1"

  if (length(cat.par) == 0) {

    qrf <- ranger::ranger(formula(data), data, quantreg = TRUE,
                          keep.inbag = T, min.node.size = 20)

    return(sapply(1:nrow(data),
           function(id) ecdf(qrf$random.node.values.oob[id,])(data$X1[id])))

  } else {

    remaining.idx <- rep(FALSE,nrow(data))
    quantiles <- rep(NA, nrow(data))

    if (length(cat.par) == 1) {
      cat.par.values <- as.matrix(levels(data[, cat.par]))

    } else {

      cat.par.values <- expand.grid(lapply(data[, cat.par], levels))

    }

    for (i in 1:dim(cat.par.values)[1]) {

      curr.idx = rep(TRUE, nrow(data))

      for(j in 1:length(cat.par)) {

        curr.idx <- curr.idx & (data[, cat.par[j]] == cat.par.values[i,j])

      }

      if (sum(curr.idx) < 100) {

        remaining.idx[curr.idx] <- TRUE
        next

      }

      curr.data <- data[curr.idx, ]
      qrf <- ranger::ranger(formula(curr.data), curr.data, quantreg = TRUE,
                            keep.inbag = T, min.node.size = 20)
      quantiles[curr.idx] <- sapply(1:nrow(curr.data),
                                    function(id) ecdf(qrf$random.node.values.oob[id,])(curr.data$X1[id]))
    }

    if (sum(remaining.idx) > 0) {

      curr.data <- data[remaining.idx, ]
      qrf <- ranger::ranger(formula(curr.data), curr.data, quantreg = TRUE,
                            keep.inbag = T, min.node.size = 20)
      quantiles[remaining.idx] <- sapply(1:nrow(curr.data),
                                         function(id) ecdf(qrf$random.node.values.oob[id,])(curr.data$X1[id]))

    }

    return(quantiles)

  }

}

ComputeCFVals <- function(data, cf.data, protect.A) {

  mtry <- sum(colnames(data) != protect.A) - 1 # want large mtry for these steps

  cf.forest <- ranger::ranger(formula = formula(data[, colnames(data) != protect.A]),
                              data = data, mtry = mtry)  # use baseline data to learn the inverse quant. func
  cf.values <- predict(cf.forest, data = cf.data)$predictions

  return(cf.values)

}

InferCtf <- function(data, cf.parents, ind, A.root, cat.parents, protect.A,
   quant.method = "forest") {

  if (quant.method == "forest2") {

    est.quants <- Quantiles(data, cat.parents)

    inv.quant.data <- cbind(data, est.quants)
    inv.quant.data <- inv.quant.data[ind, ]
    cf.parents <- cbind(cf.parents, est.quants[!ind])

    colnames(cf.parents) <- colnames(inv.quant.data)[-1]

    return(ComputeCFVals(inv.quant.data, cf.parents, protect.A))

  }

  CtfAAP(data, cf.parents, ind, A.root, quant.method)

}
