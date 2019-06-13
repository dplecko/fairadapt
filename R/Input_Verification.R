CorrectInput <- function(formula, train.data, test.data, adjacency.matrix,
                         protected.attribute, resolving.variables = NULL) {
  if (!WithinRange(adjacency.matrix)) {
    stop("Adjacency matrix has entries different from 0,1")
  }
  if (is.null(colnames(adjacency.matrix)) | is.null(rownames(adjacency.matrix))) {
    stop("Row or column names of the adjacency matrix missing")
  }
  if (!setequal(rownames(adjacency.matrix), colnames(adjacency.matrix))) {
    stop("Row and column names of the adjacency matrix do not match")
  }
  if (length(colnames(adjacency.matrix)) != length(unique(colnames(adjacency.matrix)))) {
    stop("Column names are not unique")
  }
  if (length(rownames(adjacency.matrix)) != length(unique(rownames(adjacency.matrix)))) {
    stop("Row names are not unique")
  }
  adjacency.matrix <- adjacency.matrix[colnames(adjacency.matrix), ]
  if (!IsAcyclic(adjacency.matrix)) {
    stop("The specified DAG is not acylic")
  }
  if (!is.null(resolving.variables)) {
    if (sum(!is.element(resolving.variables,colnames(adjacency.matrix)))) {
      stop("There are resolving variables not appearing in the specified DAG")
    }
  }
  if (!is.element(protected.attribute,colnames(adjacency.matrix))) {
    stop("Protected attribute missing in the specified DAG")
  }
  train.data <- model.frame(formula, train.data)
  test.data <- test.data[, colnames(train.data)[-1]]
  if (sum(is.na(train.data))) {
    stop("NA values in the training data")
  }
  if (sum(is.na(test.data))) {
    stop("NA values in the test set")
  }
  if (sum(!is.element(colnames(train.data),colnames(adjacency.matrix)))) {
    stop("There are covariates not appearing in the specified DAG")
  }
}

WithinRange <- function(mat) {
  matrix.size <- dim(mat)
  num.odd.entries <- sum(!is.element(mat,c(0,1)))
  return(num.odd.entries == 0)
}

IsAcyclic <- function(mat) {
  matrix.size <- dim(mat)
  num.walks <- mat
  for (i in 1:matrix.size[1]) {
    num.walks <- mat + num.walks %*% mat
  }
  acyclic <- (sum(diag(num.walks)) == 0)
  return(acyclic)
}
