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
