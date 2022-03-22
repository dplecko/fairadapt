
getDescendants <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) {

    pos <- which(top.ord == var)

    if (pos == length(top.ord)) {
      return(character(0))
    }

    return(top.ord[seq.int(pos + 1L, length(top.ord))])
  }

  if (is.null(adj.mat)) {
    return(top.ord[seq.int(which(top.ord == var) + 1L, length(top.ord))])
  }

  num.walks <- adj.mat

  for (i in seq_row(adj.mat)) {
    num.walks <- adj.mat + num.walks %*% adj.mat
  }

  colnames(adj.mat)[num.walks[var, ] > 0]
}

getAncestors <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) {

    pos <- which(top.ord == var)

    if (pos == 1) {
      return(character(0L))
    }

    return(top.ord[seq_len(pos - 1L)])
  }

  num.walks <- adj.mat

  for (i in seq_row(adj.mat)) {
    num.walks <- adj.mat + num.walks %*% adj.mat
  }

  colnames(adj.mat)[num.walks[, var] > 0]
}

getParents <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) {
    return(getAncestors(var, adj.mat, top.ord))
  }

  if (length(var) > 1) {
    return(Reduce(c, lapply(var, getParents, adj.mat)))
  }

  unique(row.names(adj.mat)[adj.mat[, var] == 1])
}

getChildren <- function(var, adj.mat, top.ord = NULL) {

  if (is.null(adj.mat)) {
    return(getDescendants(var, adj.mat, top.ord))
  }

  if (length(var) > 1) {
    return(Reduce(c, lapply(var, getChildren, adj.mat)))
  }

  unique(row.names(adj.mat)[adj.mat[var, ] == 1])
}

confoundedComponent <- function(var, cfd.matrix) {

  assert_that(identical(cfd.matrix, t(cfd.matrix)))

  num.walks <- cfd.matrix

  cfd.matrix <- cfd.matrix + diag(dim(cfd.matrix)[1L])

  for (i in seq_len(nrow(cfd.matrix) + 1L)) {
    num.walks <- cfd.matrix + num.walks %*% cfd.matrix
  }

  colnames(cfd.matrix)[num.walks[var, ] > 0]
}

adjustmentSet <- function(var, adj.mat, cfd.mat, top.ord) {

  if (is.null(adj.mat)) {
    return(top.ord[seq_len(which(top.ord == var) - 1)])
  }

  cc <- confoundedComponent(var, cfd.mat)

  intersect(
    getAncestors(var, adj.mat),
    union(
      cc,
      getParents(cc, adj.mat)
    )
  )
}

topologicalOrdering <- function(adj.mat) {

  nrw <- nrow(adj.mat)
  num.walks <- adj.mat

  for (i in seq_len(nrw + 1L)) {
    num.walks <- adj.mat + num.walks %*% adj.mat
  }

  comparison.matrix <- num.walks > 0

  top.order <- colnames(adj.mat)

  for (i in seq_len(nrw - 1L)) {

    for (j in seq.int(i + 1L, nrw)) {

      if (comparison.matrix[top.order[j], top.order[i]]) {
        top.order <- swap(top.order, i, j)
      }
    }

  }

  top.order
}

nonId <- function(iv, adj.mat, cfd.mat) {

  nonIdImpl <- function(var) {
    length(intersect(getChildren(var, adj.mat),
                     confoundedComponent(var, cfd.mat))) > 0
  }

  any(vapply(iv, nonIdImpl, logical(1L)))
}

#' Obtaining the graphical causal model (GCM)
#'
#' @param adj.mat Matrix of class `matrix` encoding the relationships in
#' the causal graph. `M[i,j] == 1L` implies the existence of an edge from
#' node i to node j.
#' @param cfd.mat Symmetric matrix of class `matrix` encoding the
#' bidirected edges in the causal graph. `M[i,j] == M[j, i] == 1L`
#' implies the existence of a bidirected edge between nodes i and j.
#' @param res.vars A vector of class `character` listing all the resolving
#' variables, which should not be changed by the adaption procedure. Default
#' value is `NULL`, corresponding to no resolving variables. Resolving
#' variables should be a subset of `colnames(adj.mat)`. Resolving
#' variables are marked with a different color in the output.
#'
#' @return An object of class `igraph`, containing the causal graphical,
#' with directed and bidirected edges.
#'
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
#' @importFrom igraph graph_from_adjacency_matrix as_edgelist add_edges
#' @importFrom igraph E E<- V V<-
#' @export
graphModel <- function(adj.mat, cfd.mat = NULL, res.vars = NULL) {

  if (is.null(cfd.mat)) {
    cfd.mat <- matrix(0L, nrow = nrow(adj.mat), ncol = ncol(adj.mat),
                      dimnames = dimnames(adj.mat))
  } else {
    cfd.mat <- cfd.mat[colnames(adj.mat), colnames(adj.mat)]
  }

  res <- graph_from_adjacency_matrix(adj.mat)

  E(res)$curved <- 0
  E(res)$lty <- "solid"

  diag(cfd.mat) <- 0

  cfg <- graph_from_adjacency_matrix(cfd.mat)

  e.list <- as_edgelist(cfg, names = FALSE)
  curved <- (e.list[, 1] < e.list[, 2]) - 0.5

  lty <- ifelse((e.list[, 1] < e.list[, 2]), "dashed", "blank")
  res <- add_edges(res, as.vector(t(e.list)), curved = curved, lty = lty)

  E(res)$color <- "black"
  E(res)$arrow.size <- 0.35
  V(res)$color <- "white"
  V(res)$color[which(names(V(res)) %in% res.vars)] <- "red"

  res
}
