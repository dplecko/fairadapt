##' Implementation of fair data adaptation with quantile preservation (Plecko & Meinshausen 2019).
##' Uses only plain \code{R}.
##'
##' The procedure takes the training and testing data as an input, together with the causal graph given by an adjacency matrix and the list of resolving variables, which should be kept fixed during the adaptation procedure. The procedure then calculates a fair representation of the data, after which any classification method can be used. There are, however, several valid training options yielding fair predictions, and the best of them can be chosen with cross-validation. For more details we refer the user to the original paper.
##' Most of the running time is due to the quantile regression step using the ranger package.
##'
##' @title fairadapt
##' @param formula Object of class \code{formula} describing the response and the covariates.
##' @param train.data,test.data Training data & testing data, both of class \code{data.frame}.
##' @param adj.mat Matrix of class \code{matrix} encoding the relationships in the causal graph. \code{M[i,j] == 1} implies the existence of an edge from node i to node j. Must include all the variables appearing in the formula object.
##' @param protect.A A value of class \code{character} describing the binary protected attribute. Must be one of the entries of \code{colnames(adj.mat)}.
##' @param res.vars A vector of class \code{character} listing all the resolving variables, which should not be changed by the adaption procedure. Default value is \code{NULL}, corresponding to no resolving variables. Resolving variables should be a subset of the descendants of the protected attribute.
##' @param quant.method A vector of class \code{character} choosing the method used for quantile regression. Possible values are "forest", "forest2", "linear" and "nn".
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
fairadapt <- function(formula, train.data, test.data, adj.mat = NULL, cfd.mat = NULL,
                      top.ord = NULL, protect.A, res.vars = NULL, quant.method = "forest",
                      visualize.graph = TRUE) {

  # verify correctness of input
  CorrectInput(formula, train.data, test.data, adj.mat, cfd.mat, top.ord,
               protect.A, res.vars, quant.method)

  # reorder the adjacency matrix if necessary
  adj.mat <- adj.mat[colnames(adj.mat), ]

  # reorder columns and Factor-ize
  list2env(ReorderCols(formula, train.data, test.data, protect.A), envir = environment())

  # keep important parts of adjacency matrix
  adj.mat <- adj.mat[colnames(org.data), colnames(org.data)]

  if(is.null(cfd.mat) & !is.null(adj.mat)) {

    cfd.mat <- adj.mat
    cfd.mat[, ] <- 0

  }

  # construct the initial version of adapted data
  list2env(InitAdapt(org.data, protect.A), envir = environment())

  # obtain topological ordering and descendants of A
  if(is.null(top.ord)) { # Markovian / Semi-Markovian case

    top.ord <- TopologicalOrdering(adj.mat)
    A.des <- GetDescendants(protect.A, adj.mat)
    A.root <- (length(GetParents(protect.A, adj.mat)) == 0L) &
      (length(ConfoundedComponent(protect.A, cfd.mat)) == 1L)

    if (visualize.graph) {

      ig <- VisualizeGraph(adj.mat, cfd.mat)
      plot(ig)

    }

    # fail if NonID
    if (NonID(c(protect.A, res.vars), adj.mat, cfd.mat))
      stop("The desired intervention is non-identifiable")

  } else { # Topological Ordering case

    A.des <- GetDescendants(protect.A, adj.mat, top.ord)
    A.root <- top.ord[1] == protect.A

  }

  # main procedure part
  for (curr.var in top.ord[(which(top.ord == protect.A) + 1):length(top.ord)]) {

    # check if this variable is skipped

    # need to change this for the topological order approach  / also for when A is not root

    changed.parents <- intersect(GetParents(curr.var, adj.mat, top.ord), union(A.des, protect.A))
    if (sum(!is.element(changed.parents, res.vars)) == 0) res.vars <- c(res.vars, curr.var)
    if (is.element(curr.var, res.vars) | !is.element(curr.var, A.des)) next


    discrete <- FALSE
    curr.parents <- AdjustmentSet(curr.var, adj.mat, cfd.mat, top.ord)
    curr.cat.parents <-
      curr.parents[sapply(1:length(curr.parents),
                          function(x) is.factor(train.data[, curr.parents[x]]))]

    row.idx <- rep(TRUE, full.len)

    if (curr.var == colnames(train.data)[1]) row.idx[-(1:train.len)] <- FALSE

    # check if Discrete
    if (length(unique(org.data[, curr.var])) < 100 |
        is.factor(org.data[, curr.var])) {

      list2env(EncodeDiscrete(
          org.data[row.idx, 1], org.data[row.idx, curr.var]
        ), envir = environment()
      )

      org.data[, curr.var] <- adapt.data[, curr.var] <-
        MakeLength(int.enc, train.len, full.len)

    }

    ### perform the Adaptation
    curr.adapt.data <-
      org.data[row.idx, c(curr.var, curr.parents), drop = F]
    curr.cf.parents <-
      adapt.data[!base.ind & row.idx, curr.parents, drop = F]

    adapt.data[!base.ind & row.idx, curr.var] <-
      InferCtf(data = curr.adapt.data, cf.parents = curr.cf.parents, ind = base.ind[row.idx],
        A.root = A.root, cat.parents = curr.cat.parents, protect.A = protect.A,
        quant.method = quant.method)

    # check if there exists a resolving ancestor
    ancestors <- GetAncestors(curr.var, adj.mat, top.ord)
    res.anc <- (sum(is.element(ancestors, res.vars)) > 0)

    # enforce Marginal Matching if there is no resolving ancestor & discrete
    if (discrete & !res.anc & A.root) {

      adapt.data[row.idx, curr.var] <-
        MarginalMatching(adapt.data[row.idx, curr.var], base.ind[row.idx])

    }

    # if discrete, recode back to discrete or factor
    if (discrete) {

      adapt.var <- MakeLength(
        DecodeDiscrete(adapt.data[row.idx, curr.var], cat.enc, unique.values),
        train.len, full.len
      )

      org.var <- MakeLength(
        DecodeDiscrete(org.data[row.idx, curr.var], cat.enc, unique.values),
        train.len, full.len
      )

      if (is.factor(train.data[, curr.var])) {

        adapt.var <- as.factor(adapt.var)
        org.var <- as.factor(org.var)

      }

      adapt.data[, curr.var] <- adapt.var
      org.data[, curr.var] <- org.var

    }

  }

  return(
    list(
      adapt.data[1:train.len, ],
      adapt.data[-(1:train.len),-1]
    )
  )

}
