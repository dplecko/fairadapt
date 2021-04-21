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

    offending.cols <- 1 + which(vapply(2:ncol(data),
                                  function(x) length(unique(data[, x])),
                                  1L) == 1)
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

  if(!is.null(quantiles)) ctf.values <-
    vapply(1:nrow(newdata), function(x) quantile(quantiles[x, ], newU[x]),
           0.0)

  ctf.values

}

GetDescendants <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) {

    pos <- which(top.ord == var)
    if(pos == length(top.ord)) return(character(0))

    return(top.ord[(pos + 1):length(top.ord)])

  }

  if (is.null(adj.mat)) return(top.ord[(which(top.ord == var) + 1 ):length(top.ord)])

  matrix.size <- dim(adj.mat)
  num.walks <- adj.mat

  for (i in 1:matrix.size[1]) {

    num.walks <- adj.mat + num.walks %*% adj.mat

  }

  descendant.indicators <- num.walks[var, ] > 0
  descendants <- colnames(adj.mat)[descendant.indicators]

  return(descendants)

}

GetAncestors <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) {

    pos <- which(top.ord == var)
    if(pos == 1) return(character(0))

    return(top.ord[1:(pos - 1)])

  }

  matrix.size <- dim(adj.mat)
  num.walks <- adj.mat

  for (i in 1:matrix.size[1]) {

    num.walks <- adj.mat + num.walks %*% adj.mat

  }

  ancestor.indicators <- num.walks[, var] > 0
  ancestors <- colnames(adj.mat)[ancestor.indicators]

  return(ancestors)

}

GetParents <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) return(GetAncestors(var, adj.mat, top.ord))

  if (length(var) > 1) return(Reduce(c, lapply(var, GetParents, adj.mat)))

  parent.indicators <- adj.mat[, var] == 1
  parents <- row.names(adj.mat)[parent.indicators]

  return(unique(parents))

}

GetChildren <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) return(GetDescendants(var, adj.mat, top.ord))

  if (length(var) > 1) return(Reduce(c, lapply(var, GetChildren, adj.mat)))

  ch.idx <- adj.mat[var, ] == 1
  children <- row.names(adj.mat)[ch.idx]

  return(unique(children))

}

ConfoundedComponent <- function(var, cfd.matrix) {

  assertthat::assert_that(identical(cfd.matrix, t(cfd.matrix)))

  matrix.size <- dim(cfd.matrix) + 1
  num.walks <- cfd.matrix

  cfd.matrix <- cfd.matrix + diag(dim(cfd.matrix)[1])

  for (i in 1:matrix.size[1]) {

    num.walks <- cfd.matrix + num.walks %*% cfd.matrix

  }

  cc.idx <- num.walks[var, ] > 0
  cc <- colnames(cfd.matrix)[cc.idx]

  cc

}

AdjustmentSet <- function(var, adj.mat, cfd.mat, top.ord) {

  if(is.null(adj.mat)) return(top.ord[1:(which(top.ord == var)-1)])

  cc <- ConfoundedComponent(var, cfd.mat)

  intersect(
    GetAncestors(var, adj.mat),
    union(
      cc,
      GetParents(cc, adj.mat)
    )
  )

}

TopologicalOrdering <- function(adj.mat) {

  matrix.size <- dim(adj.mat)
  num.walks <- adj.mat

  for (i in 1:(matrix.size[1]+1)) {

    num.walks <- adj.mat + num.walks %*% adj.mat

  }

  comparison.matrix <- num.walks > 0

  top.order <- colnames(adj.mat)

  for (i in 1:(matrix.size[1]-1)) {

    for (j in (i+1):matrix.size[1]) {

      if (comparison.matrix[top.order[j], top.order[i]]) {

        top.order <- Swap(top.order, i, j)

      }
    }

  }

  return(top.order)

}

NonID <- function(iv, adj.mat, cfd.mat) {

  any(
    sapply(iv, function(var) length(intersect(
        GetChildren(var, adj.mat), ConfoundedComponent(var, cfd.mat)
      )
    ) > 0)
  )

}

#' Obtaining the graphical causal model (GCM)
#'
#'
#' @param adj.mat Matrix of class \code{matrix} encoding the relationships in
#' the causal graph. \code{M[i,j] == 1L} implies the existence of an edge from
#' node i to node j.
#' @param cfd.mat Symmetric matrix of class \code{matrix} encoding the
#' bidirected edges in the causal graph. \code{M[i,j] == M[j, i] == 1L}
#' implies the existence of a bidirected edge between nodes i and j.
#' @param res.vars A vector of class \code{character} listing all the resolving
#' variables, which should not be changed by the adaption procedure. Default
#' value is \code{NULL}, corresponding to no resolving variables. Resolving
#' variables should be a subset of \code{colnames(adj.mat)}. Resolving
#' variables are marked with a different color in the output.
#' @return An object of class \code{igraph}, containing the causal graphical,
#' with directed and bidirected edges.
#' @examples
#' adj.mat <- cfd.mat <- array(0L, dim = c(3, 3))
#' colnames(adj.mat) <- rownames(adj.mat) <-
#'   colnames(cfd.mat) <- rownames(cfd.mat) <- c("A", "X", "Y")
#'
#' adj.mat["A", "X"] <- adj.mat["X", "Y"] <-
#'   cfd.mat["X", "Y"] <- cfd.mat["Y", "X"] <- 1L
#'
#' gcm <- graphModel(adj.mat, cfd.mat, res.vars = "X")
#'
#' @export
graphModel <- function(adj.mat, cfd.mat = NULL, res.vars = NULL) {

  if(is.null(cfd.mat)) {
    cfd.mat <- adj.mat
    cfd.mat[, ] <- 0L
  }

  cfd.mat <- cfd.mat[colnames(adj.mat), colnames(adj.mat)]

  g <- igraph::graph_from_adjacency_matrix(adj.mat)
  igraph::E(g)$curved <- 0
  igraph::E(g)$lty <- "solid"
  diag(cfd.mat) <- 0

  cfg <- igraph::graph_from_adjacency_matrix(cfd.mat)
  e.list <- igraph::as_edgelist(cfg, names = F)
  curved <- (e.list[, 1] < e.list[, 2]) - 0.5
  lty <- ifelse((e.list[, 1] < e.list[, 2]), "dashed", "blank")
  g <- igraph::add_edges(g, as.vector(t(e.list)), curved = curved, lty = lty)


  igraph::E(g)$color <- "black"
  igraph::E(g)$arrow.size <- 0.35
  igraph::V(g)$color <- "white"
  igraph::V(g)$color[which(names(igraph::V(g)) %in% res.vars)] <- "red"
  g

}
