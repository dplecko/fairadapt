rangerQuants <- function(data, A.root, ind) {

  if (A.root)
    return(
      structure(
        list(class0 = rangerQuants(data[ind, ], FALSE, NULL),
        class1 = rangerQuants(data[!ind, ], FALSE, NULL)),
        class = "rangersplit"
      )
    )

  ranger::ranger(formula(data), data = data, quantreg = T,
                 keep.inbag = T, min.node.size = 20)

}

linearQuants <- function(data, A.root, ind) {

  if (A.root)
    return(
      structure(
        list(class0 = linearQuants(data[ind, ], FALSE, NULL),
        class1 = linearQuants(data[!ind, ], FALSE, NULL)),
        class = "quantregsplit"
      )
    )

  offending.cols <- 1 + which(vapply(2:ncol(data),
                                     function(x) length(unique(data[, x])),
                                     1L) == 1)
  keep.cols <- which(!(1:ncol(data) %in% offending.cols))

  if (length(offending.cols) == (ncol(data)-1)) {

    form <- as.formula(paste(names(data)[1L], "~ 1"))
    object <- quantreg::rq(form, data = data,
                           tau = c(0.001, seq(0.005, 0.995, by = 0.01), 0.999))

  } else {

    object <- quantreg::rq(formula(data[, keep.cols]), data = data,
                           tau = c(0.001, seq(0.005, 0.995, by = 0.01), 0.999))

  }

  object

}

mcqrnnQuants <- function(data, A.root, ind) {

  data.matrix <- matrix(as.numeric(unlist(data)), nrow=nrow(data))
  object <- qrnn::mcqrnn.fit(x = data.matrix[, -1, drop = FALSE],
                             y = matrix(data.matrix[, 1], ncol = 1),
                             tau = seq(0.005, 0.995, by = 0.01),
                             n.trials = 1, iter.max = 500, trace = FALSE)
  structure(
    object, class = "mcqrnnobj"
  )
}

#' @export
computeQuants <- function(x, data, newdata, ind, ...) {
  UseMethod("computeQuants", x)
}

#' @export
computeQuants.ranger <- function(x, data, newdata, ind, ...) {

  # GetQuants
  empirical <- x$random.node.values.oob
  quantiles <- predict(x, data = newdata, type = "quantiles",
                       what = function(x) x)$predictions

  inferQuant(data, empirical, quantiles, ind)
}

#' @export
computeQuants.rangersplit <- function(x, data, newdata, ind, ...) {

  # GetQuants
  empirical <- x$class1$random.node.values.oob
  quantiles <- predict(x$class0, data = newdata, type = "quantiles",
                       what = function(res) res)$predictions

  inferQuantsplit(data, empirical, quantiles, ind)

}

#' @export
computeQuants.quantreg <- function(x, data, newdata, ind, ...) {

  empirical <- predict(x, newdata = data[, -1, drop = FALSE])
  quantiles <- predict(x, newdata = newdata)

  inferQuant(data, empirical, quantiles, ind)

}

#' @export
computeQuants.quantregsplit <- function(x, data, newdata, ind, ...) {

  empirical <- predict(x$class1, newdata = data[!ind, -1, drop = FALSE])
  quantiles <- predict(x$class0, newdata = newdata)

  inferQuantsplit(data, empirical, quantiles, ind)

}

#' @export
computeQuants.mcqrnnobj <- function(x, data, newdata, ind, ...) {

  xmat <- matrix(as.numeric(unlist(data[, -1, drop = FALSE])), nrow=nrow(data))
  empirical <- qrnn::mcqrnn.predict(x = xmat, parms = x)

  newx <- matrix(as.numeric(unlist(newdata)), ncol = ncol(newdata))
  quantiles <- qrnn::mcqrnn.predict(x = newx, parms = x)

  inferQuant(data, empirical, quantiles, ind)

}

inferQuant <- function(data, empirical, quantiles, ind) {

  eval <- data[, 1]
  U.hat <- vapply(seq_len(nrow(data)), function(x) ecdf(empirical[x, ]) (eval[x]),
                  numeric(1L))

  newU <- U.hat[!ind]

  vapply(seq_len(nrow(quantiles)), function(x) quantile(quantiles[x, ], newU[x]),
         numeric(1L))

}

inferQuantsplit <- function(data, empirical, quantiles, ind) {

  eval <- data[!ind, 1]
  U.hat <- vapply(seq_len(nrow(data[!ind, ])), function(x) ecdf(empirical[x, ]) (eval[x]),
                  numeric(1L))

  newU <- U.hat
  vapply(seq_len(nrow(quantiles)), function(x) quantile(quantiles[x, ], newU[x]),
         numeric(1L))

}
