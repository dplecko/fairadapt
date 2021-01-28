
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

  if (is.null(adj.mat)) return(GetDescendants(var, ajd.mat, top.ord))

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

VisualizeGraph <- function(adj.mat, cfd.mat) {

  g <- graph_from_adjacency_matrix(adj.mat)
  E(g)$curved <- 0
  E(g)$lty <- "solid"
  diag(cfd.mat) <- 0

  cfg <- graph_from_adjacency_matrix(cfd.mat)
  e.list <- as_edgelist(cfg, names = F)
  curved <- (e.list[, 1] < e.list[, 2]) - 0.5
  lty <- ifelse((e.list[, 1] < e.list[, 2]), "dashed", "blank")
  g <- add_edges(g, as.vector(t(e.list)), curved = curved, lty = lty)


  E(g)$color <- "black"
  E(g)$arrow.size <- 0.35
  V(g)$color <- "white"
  g

}
