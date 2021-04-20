##' Implementation of fair data adaptation with quantile preservation
##' (Plecko & Meinshausen 2019).
##' Uses only plain \code{R}.
##'
##' The procedure takes the training and testing data as an input, together with
##' the causal graph given by an adjacency matrix and the list of resolving
##' variables, which should be kept fixed during the adaptation procedure. The
##' procedure then calculates a fair representation of the data, after which
##' any classification method can be used. There are, however, several valid
##' training options yielding fair predictions, and the best of them can be
##' chosen with cross-validation. For more details we refer the user to the
##' original paper. Most of the running time is due to the quantile regression
##' step using the ranger package.
##'
##' @title fairadapt
##' @param formula Object of class \code{formula} describing the response and
##' the covariates.
##' @param train.data,test.data Training data & testing data, both of class
##' \code{data.frame}.
##' @param adj.mat Matrix of class \code{matrix} encoding the relationships in
##' the causal graph. \code{M[i,j] == 1L} implies the existence of an edge from
##' node i to node j. Must include all the variables appearing in the formula
##' object.
##' @param cfd.mat Symmetric matrix of class \code{matrix} encoding the
##' bidirected edges in the causal graph. \code{M[i,j] == M[j, i] == 1L}
##' implies the existence of a bidirected edge between nodes i and j. Must
##' include all the variables appearing in the formula object.
##' @param top.ord A vector of class \code{character} describing the
##' topological ordering of the causal graph. Default value is \code{NULL},
##' but this argument must be supplied if \code{adj.mat} is not specified.
##' Also must include all the variables appearing in the formula object.
##' @param protect.A A value of class \code{character} describing the binary
##' protected attribute. Must be one of the entries of \code{colnames(adj.mat)}.
##' @param res.vars A vector of class \code{character} listing all the resolving
##'  variables, which should not be changed by the adaption procedure. Default
##'  value is \code{NULL}, corresponding to no resolving variables. Resolving
##'  variables should be a subset of the descendants of the protected attribute.
##' @param quant.method A vector of class \code{character} choosing the method
##' used for quantile regression. Possible values are "forest", "forest2",
##' "linear" and "nn".
##' @param visualize.graph A \code{logical} indicating whether the causal graph
##' should be plotted upon calling the \code{fairadapt()} function. Default
##' value is \code{FALSE}.
##' @return An object of class \code{fairadapt}, containing the original and
##' adapted training and testing data, together with the causal graph and some
##' additional meta-information.
##' @examples
##' uni.adj.mat <- array(0, dim = c(4, 4))
##' colnames(uni.adj.mat) <- rownames(uni.adj.mat) <-
##'   c("gender", "edu", "test", "score")
##'
##' uni.adj.mat["gender", c("edu", "test")] <-
##'   uni.adj.mat["edu", c("test", "score")] <-
##'   uni.adj.mat["test", "score"] <- 1L
##'
##' FA <- fairadapt(score ~ .,
##'   train.data = uni_admission[1:100, ],
##'   test.data = uni_admission[101:150, ],
##'   adj.mat = uni.adj.mat, protect.A = "gender")
##'
##' FA
##'
##' @author Drago Plecko
##' @references
##' Plecko, D. & Meinshausen, N. (2019).
##' Fair Data Adaptation with Quantile Preservation \cr
##' @import stats
##' @export
fairadapt <- function(formula, train.data, test.data,
  adj.mat = NULL, cfd.mat = NULL, top.ord = NULL,
  protect.A, res.vars = NULL,
  quant.method = "forest", visualize.graph = FALSE) {

  # verify correctness of input
  CorrectInput(formula, train.data, test.data, adj.mat, cfd.mat, top.ord,
               protect.A, res.vars, quant.method)

  # reorder the adjacency matrix if necessary
  adj.mat <- adj.mat[colnames(adj.mat), ]

  # reorder columns and Factor-ize
  list2env(ReorderCols(formula, train.data, test.data, protect.A),
    envir = environment())

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

    ig <- graphModel(adj.mat, cfd.mat)

    if (visualize.graph) {

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
  # seq.int()
  for (curr.var in top.ord[(which(top.ord == protect.A) + 1):length(top.ord)]) {

    # check if this variable is skipped

    # must change for topological order approach/also for when A is not root

    changed.parents <- intersect(GetParents(curr.var, adj.mat, top.ord),
                                 union(A.des, protect.A))
    if (sum(!is.element(changed.parents, res.vars)) == 0)
      res.vars <- c(res.vars, curr.var)
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
      InferCtf(data = curr.adapt.data, cf.parents = curr.cf.parents,
        ind = base.ind[row.idx], A.root = A.root,
        cat.parents = curr.cat.parents, protect.A = protect.A,
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

  structure(list(
    adapt.train = adapt.data[1:train.len, ],
    adapt.test = adapt.data[-(1:train.len),-1],
    train = train.data,
    test = test.data,
    base.ind = base.ind,
    formula = formula,
    res.vars = res.vars,
    protect.A = protect.A,
    graph = ig
  ), class = "fairadapt")

}
