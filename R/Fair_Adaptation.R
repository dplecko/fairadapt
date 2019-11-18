##' Implementation of fair data adaptation with quantile preservation (Plecko & Meinshausen 2019).
##' Uses only plain \code{R}.
##'
##' The procedure takes the training and testing data as an input, together with the causal graph given by an adjacency matrix and the list of resolving variables, which should be kept fixed during the adaptation procedure. The procedure then calculates a fair representation of the data, after which any classification method can be used. There are, however, several valid training options yielding fair predictions, and the best of them can be chosen with cross-validation. For more details we refer the user to the original paper.
##' Most of the running time is due to the quantile regression step using the ranger package.
##'
##' @title fairadapt
##' @param formula Object of class \code{formula} describing the response and the covariates.
##' @param train.data,test.data Training data & testing data, both of class \code{data.frame}.
##' @param adj.mat Matrix of class \code{matrix} encoding the relationships in the causal graph. \code{M[i,j] == 1} implies the existence of an edge from node i to node j. Must include all the
##' @param protect.A A value of class \code{character} describing the binary protected attribute. Must be one of the entries of \code{colnames(adj.mat)}.
##' @param res.vars A vector of class \code{character} listing all the resolving variables, which should not be changed by the adaption procedure. Default value is \code{NULL}, corresponding to no resolving variables. Resolving variables should be a subset of the descendants of the protected attribute.
##' @return A \code{list} of length two. The two elements of the list are of class \code{data.frame} and contain the adapted training and testing data respectively.
##' @examples
##'
##' \dontshow{
##' adjacency.matrix <- array(0, dim = c(3,3))
##' colnames(adjacency.matrix) <- c("A","Y","X")
##' rownames(adjacency.matrix) <- colnames(adjacency.matrix)
##' adjacency.matrix["A", "X"] <- 1
##' adjacency.matrix["X", "Y"] <- 1
##' DataGen <- function(n) {
##'  expit <- function(x) return(exp(x)/(1+exp(x)))
##'  A <- rbinom(n, size = 1, prob = 0.5)
##'  coeff <- 1 / 4
##'  dev <- 1
##'  X <-  -A*coeff + coeff/2 + rnorm(n, sd = dev)
##'  Y <- rbinom(n, size = 1, prob = expit((X)))
##'  df <- data.frame(cbind(Y,A,X))
##'  colnames(df) <- c("Y","A","X")
##'  return(df)
##' }
##' fairadapt(Y ~ ., train.data = DataGen(100), test.data = DataGen(100),
##'           adj.mat = adjacency.matrix, protect.A = "A")
##' }
##'
##' library(fairadapt)
##' n1 <- n2 <- 100
##' n <- n1 + n2
##' A <- rbinom(n, size = 1, prob = 0.5)
##' X1 <- rnorm(n) + (A-1)
##' X2 <- rnorm(n) + (2*A-1)
##' Y <- rbinom(n, size = 1, prob = exp(X1+X2)/(1+exp(X1+X2)))
##' data <- data.frame(cbind(A, X1, X2, Y))
##' adjacency.matrix <- array(0, dim = c(4,4))
##' colnames(adjacency.matrix) <- rownames(adjacency.matrix) <- c("A", "X1", "X2", "Y")
##' adjacency.matrix["A", c("X1", "X2")] <- 1
##' adjacency.matrix[c("X1", "X2"), "Y"] <- 1
##' L <- fairadapt(Y ~ ., train.data = data[1:n1, ], test.data = data[-(1:n1), ],
##'                protect.A = "A", adj.mat = adjacency.matrix, res.vars = "X1")
##' \donttest{
##' library(fairadapt)
##'
##' # UCI Adult example
##' L <- fairadapt(income ~ ., train.data = adult.train,
##'                test.data = adult.test, protect.A = "sex",
##'                adj.mat = adjacency.matrix)
##' adjusted.train.data <- L[[1]]
##' adjusted.test.data <- L[[2]]
##' }
##' @author Drago Plecko
##' @references
##' Plecko, D. & Meinshausen, N. (2019). Fair Data Adaptation with Quantile Preservation \cr
##' @import stats
##' @export
fairadapt <- function(formula, train.data, test.data, adj.mat,
                      protect.A, res.vars = NULL) {
  # verify correctness of input
  CorrectInput(formula, train.data, test.data, adj.mat,
               protect.A, res.vars)

  # reorder the adjacency matrix if necessary
  adj.mat <- adj.mat[colnames(adj.mat), ]

  # keep only the relevant columns
  train.data[, protect.A] <- as.factor(train.data[, protect.A])
  test.data[, protect.A] <- as.factor(test.data[, protect.A])
  train.data <- model.frame(formula, train.data)
  test.data <- test.data[, colnames(train.data)[-1]]
  test.data <- cbind(NA, test.data)
  colnames(test.data) <- colnames(train.data)
  adj.mat <- adj.mat[colnames(train.data), colnames(train.data)]
  org.data <- rbind(train.data,test.data)
  train.length <- dim(train.data)[1]
  full.length <- dim(org.data)[1]

  # construct the initial version of adapted data
  adapt.data <- org.data
  baseline.level <- levels(train.data[, protect.A])[1]
  base.ind <- org.data[, protect.A] == baseline.level
  adapt.data[, protect.A] <- factor(baseline.level,
                                    levels = levels(train.data[, protect.A]))
  # obtain topological ordering and descendants of A
  topological.order <- TopologicalOrdering(adj.mat)
  A.descendants <- GetDescendants(protect.A, adj.mat)

  # main procedure part
  for (curr.var in topological.order) {
    if (is.element(curr.var, res.vars) |
        !is.element(curr.var, A.descendants)) {
      next
    }
    discrete <- FALSE
    curr.parents <- GetParents(curr.var, adj.mat)
    curr.cat.parents <-
      curr.parents[sapply(1:length(curr.parents),
                          function(x) is.factor(train.data[, curr.parents[x]]))]
    changed.parents <- intersect(curr.parents, union(A.descendants, protect.A))
    if (sum(!is.element(changed.parents, res.vars)) == 0) {
      next
    }
    row.idx <- rep(TRUE,full.length)
    if (curr.var == colnames(train.data)[1]) {
      row.idx[-(1:train.length)] <- FALSE
    }

    if (length(unique(org.data[, curr.var])) < 100 |
        is.factor(org.data[, curr.var])) {
      discrete = TRUE
      unique.values <- unique(org.data[, curr.var])  # need to encode all appearing discrete values
      if (is.factor(org.data[, curr.var])) {
        unique.values <- as.character(unique.values[!is.na(unique.values)])
      }

      cat.encoding <- CategoricalEncoding(org.data[row.idx, ], curr.var)
      integer.encoding <- rep(NA, full.length)
      for (i in 1:length(unique.values)) {
        integer.encoding[(org.data[, curr.var] ==
                         unique.values[i]) & row.idx] <- cat.encoding[i]
      }
      integer.encoding <- integer.encoding + runif(full.length,-0.5,0.5)
      org.data[, curr.var] <- integer.encoding
      adapt.data[, curr.var] <- org.data[, curr.var]
    }

    curr.adapt.data <- org.data[row.idx,
                                c(curr.var,GetParents(curr.var, adj.mat))]
    curr.cf.parents <- as.data.frame(adapt.data[!base.ind & row.idx,
                                                GetParents(curr.var, adj.mat)])
    est.quants <- Quantiles(curr.adapt.data, curr.cat.parents)

    inv.quant.data <- cbind(curr.adapt.data, est.quants)
    inv.quant.data <- inv.quant.data[base.ind & row.idx, ]
    curr.cf.parents <- cbind(curr.cf.parents,
                             est.quants[!base.ind & row.idx])
    colnames(curr.cf.parents) <- colnames(inv.quant.data)[-1]
    curr.cf.values <- ComputeCFVals(inv.quant.data,
                                    curr.cf.parents,
                                    protect.A)

    adapt.data[!base.ind & row.idx, curr.var] <- curr.cf.values

    # check if there exists a resolving ancestor
    ancestors <- GetAncestors(curr.var, adj.mat)
    resolving.ancestor <- (sum(is.element(ancestors, res.vars)) > 0)
    if (discrete & !resolving.ancestor) {
      # enforce marginal matching
      adapt.data[, curr.var] <- MarginalMatching(adapt.data[, curr.var],
                                                 base.ind, row.idx)
      org.data[, curr.var] <- round(org.data[, curr.var])
      # convert back to original values or factors
      int.decode <- rep(NA, full.length)
      int.decode.adapt <- rep(NA, full.length)
      for (i in 1:length(unique.values)) {
        int.decode[(org.data[, curr.var] ==
                   cat.encoding[i]) & row.idx] <- unique.values[i]
        int.decode.adapt[
          (adapt.data[, curr.var] == cat.encoding[i]) &
                 row.idx] <- unique.values[i]
      }
      org.data[, curr.var] <- int.decode
      adapt.data[, curr.var] <- int.decode.adapt
      if (is.factor(train.data[, curr.var])) {
        org.data[, curr.var] <- as.factor(org.data[, curr.var])
        adapt.data[, curr.var] <- as.factor(adapt.data[, curr.var])
      }
    }
    else if (discrete & resolving.ancestor) {
      adapt.data[, curr.var] <- round(adapt.data[, curr.var])
      org.data[, curr.var] <- round(org.data[, curr.var])

      int.decode <- rep(NA, full.length)
      int.decode.adapt <- rep(NA, full.length)
      for (i in 1:length(unique.values)) {
        int.decode[(org.data[, curr.var] ==
                    cat.encoding[i]) & row.idx] <- unique.values[i]
        int.decode.adapt[(adapt.data[, curr.var] ==
                          cat.encoding[i]) & row.idx] <- unique.values[i]
      }
      org.data[, curr.var] <- int.decode
      adapt.data[, curr.var] <- int.decode.adapt
      if (is.factor(train.data[, curr.var])) {
        org.data[, curr.var] <- as.factor(org.data[, curr.var])
        adapt.data[, curr.var] <- as.factor(adapt.data[, curr.var])
      }
    }
  }
  return(list(adapt.data[1:train.length, ],
         adapt.data[-(1:train.length),-1]))
}
MarginalMatching <- function(x, base.ind, row.idx) {
  x.baseline <- round(x[base.ind & row.idx])
  x.non.baseline <- x[!base.ind & row.idx]
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
  x[!base.ind & row.idx] <- x.non.baseline
  x[base.ind & row.idx] <- x.baseline
  x[!row.idx] <- NA
  return(x)
}
CategoricalEncoding <- function(data, variable) {
  appearing.values <- unique(data[, variable])
  cond.expect = rep(0, length(appearing.values))
  cat.encoding = rep(0, length(appearing.values))
  if (!is.factor(data[, variable])) {
    cat.encoding <- order(order(appearing.values))
  }
  else {
    appearing.values <- as.character(appearing.values)
    for (i in 1:length(appearing.values)) {
      cond.expect[i] <- mean(as.integer(data[data[, variable] == appearing.values[i], 1]), na.rm = TRUE)
      if (cond.expect[i] == 1) {
        cond.expect[i] <- cond.expect[i] + rnorm(1, sd = 0.001)  # solving ties at random
      }
    }
    for(i in 1:length(appearing.values)){
      cat.encoding[i] <- sum(cond.expect <= cond.expect[i])
    }
  }
  return(cat.encoding)
}
Quantiles <- function(data, cat.par) {
  colnames(data)[1] <- "X1"
  if ( length(cat.par) == 0 ) {
    qrf <- ranger::ranger(formula(data), data, quantreg = TRUE,
                          keep.inbag = T, min.node.size = 20)
    return(sapply(1:nrow(data),
           function(id) ecdf(qrf$random.node.values.oob[id,])(data$X1[id])))
  }
  else {
    remaining.idx <- rep(FALSE,nrow(data))
    quantiles <- rep(NA, nrow(data))
    if (length(cat.par) == 1) {
      cat.par.values <- as.matrix(levels(data[, cat.par]))
    }
    else {
      cat.par.values <- expand.grid(lapply(data[, cat.par], levels))
    }
    for (i in 1:dim(cat.par.values)[1]) {
      curr.idx = rep(TRUE, nrow(data))
      for(j in 1:length(cat.par)){
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
