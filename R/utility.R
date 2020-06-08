TopologicalOrdering <- function(adjacency.matrix) {

  matrix.size <- dim(adjacency.matrix)
  num.walks <- adjacency.matrix

  for (i in 1:(matrix.size[1]+1)) {

    num.walks <- adjacency.matrix + num.walks %*% adjacency.matrix

  }

  comparison.matrix <- num.walks > 0

  top.order <- colnames(adjacency.matrix)

  for (i in 1:(matrix.size[1]-1)) {

    for (j in (i+1):matrix.size[1]) {

      if (comparison.matrix[top.order[j], top.order[i]]) {

        top.order <- Swap(top.order,i,j)

      }
    }

  }

  return(top.order)

}

Swap <- function(x, i, j) {

  keep <- x[i]
  x[i] <- x[j]
  x[j] <- keep

  return(x)

}

expit <- function(x) return(exp(x)/(1+exp(x)))

GetDescendants <- function(variable, adjacency.matrix) {

  matrix.size <- dim(adjacency.matrix)
  num.walks <- adjacency.matrix

  for (i in 1:matrix.size[1]) {

    num.walks <- adjacency.matrix + num.walks %*% adjacency.matrix

  }

  descendant.indicators <- num.walks[variable, ] > 0
  descendants <- colnames(adjacency.matrix)[descendant.indicators]

  return(descendants)

}

GetAncestors <- function(variable, adjacency.matrix) {

  matrix.size <- dim(adjacency.matrix)
  num.walks <- adjacency.matrix

  for (i in 1:matrix.size[1]) {

    num.walks <- adjacency.matrix + num.walks %*% adjacency.matrix

  }

  ancestor.indicators <- num.walks[, variable] > 0
  ancestors <- colnames(adjacency.matrix)[ancestor.indicators]

  return(ancestors)

}

GetParents <- function(variable, adjacency.matrix) {

  parent.indicators <- adjacency.matrix[, variable] == 1
  parents <- row.names(adjacency.matrix)[parent.indicators]

  return(parents)

}

CtfAAP <- function(data, cf.parents, ind, A.parent, quant.method = "forest") {

  nosplit <- (!A.parent) | (quant.method == "nn")

  if (ncol(data) != (ncol(cf.parents)+1)) stop("Mismatch in the number of columns")

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

  return(ctf.values)

}

GetQuants <- function(data, quant.method) {

  if(quant.method == "forest") {

    object <- ranger::ranger(formula(data), data = data, quantreg = T, keep.inbag = T, min.node.size = 20)

    empirical <- object$random.node.values.oob

  } else if (quant.method == "nn") {

    data.matrix <- matrix(as.numeric(unlist(data)), nrow=nrow(data))
    object <- qrnn::mcqrnn.fit(x = data.matrix[, -1, drop = FALSE], y = matrix(data.matrix[, 1], ncol = 1),
      tau = seq(0.005, 0.995, by = 0.01),
      n.trials = 1, iter.max = 500, trace = FALSE)

    x <- matrix(as.numeric(unlist(data[, -1, drop = FALSE])), nrow=nrow(data))

    empirical <- qrnn::mcqrnn.predict(x = x, parms = object)

  } else if (quant.method == "linear") {

    offending.cols <- 1 + which(sapply(2:ncol(data), function(x) length(unique(data[, x]))) == 1)
    keep.cols <- which(!(1:ncol(data) %in% offending.cols))

    if (length(offending.cols) == (ncol(data)-1)) {

      object <- quantreg::rq(Y ~ 1, data = data, tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    } else {

      object <- quantreg::rq(formula(data[, keep.cols]), data = data,
        tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    }

    empirical <- predict(object, newdata = data[, -1, drop = FALSE])

  } else {

    stop("Unknown quant.method")

  }

  eval <- data[, 1]
  U.hat <- sapply(1:nrow(data), function(x) ecdf(empirical[x, ]) (eval[x]))

  return(U.hat)

}

InvertQ <- function(data, newdata, U, newU, quant.method) {

  if (quant.method == "forest") {

    object <- ranger::ranger(formula(data), data = data, quantreg = T, min.node.size = 20)

    quantiles <- predict(object, data = newdata, type = "quantiles", what = function(x) x)$predictions

  } else if (quant.method == "nn") {

    data.matrix <- matrix(as.numeric(unlist(data)), nrow=nrow(data))

    object <- qrnn::mcqrnn.fit(x = data.matrix[, -1, drop = FALSE], y = matrix(data.matrix[, 1], ncol = 1),
      tau = seq(0.005, 0.995, by = 0.01),
      n.trials = 1, iter.max = 500, trace = FALSE)

    x <- matrix(as.numeric(unlist(newdata)), ncol = ncol(newdata))

    quantiles <- qrnn::mcqrnn.predict(x = x, parms = object)

  } else if (quant.method == "linear") {

    offending.cols <- 1 + which(sapply(2:ncol(data), function(x) length(unique(data[, x]))) == 1)
    keep.cols <- which(!(1:ncol(data) %in% offending.cols))

    if (length(offending.cols) == (ncol(data)-1)) {

      object <- quantreg::rq(Y ~ 1, data = data, tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    } else {

      object <- quantreg::rq(formula(data[, keep.cols]), data = data,
        tau = c(0.001,seq(0.005, 0.995, by = 0.01), 0.999))

    }

    quantiles <- predict(object, newdata = newdata)

  } else {

    stop("Unknown quant.method")

  }

  if(!is.null(quantiles)) ctf.values <- sapply(1:nrow(newdata), function(x) quantile(quantiles[x, ], newU[x]))

  return(ctf.values)
}
