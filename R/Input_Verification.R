CorrectInput <- function(formula, train.data, test.data, adj.mat, cfd.mat, top.ord,
                         protect.A, resolving.variables = NULL, quant.method) {

  #assertthat::assert_that(quant.method %in% c("forest", "forest2", "linear", "nn"))

  if (is.null(adj.mat)) {

    assertthat::assert_that(!is.null(top.ord))
    ap.nms <- top.ord

  }

  else {

    assertthat::assert_that(!is.null(colnames(adj.mat)), !is.null(rownames(adj.mat)))
    assertthat::assert_that(WithinRange(adj.mat))
    assertthat::assert_that(setequal(rownames(adj.mat), colnames(adj.mat)))

    adj.mat <- adj.mat[colnames(adj.mat), ]

    assertthat::assert_that(IsAcyclic(adj.mat))

    ap.nms <- colnames(adj.mat)

  }

  assertthat::assert_that(length(ap.nms) == length(unique(ap.nms)))

  assertthat::assert_that(sum(!is.element(resolving.variables, colnames(adj.mat))) == 0)

  assertthat::assert_that(is.element(protect.A, ap.nms))

  train.data <- model.frame(formula, train.data)
  if (!is.null(test.data)) test.data <- test.data[, colnames(train.data)[-1]]

  assertthat::assert_that(sum(is.na(train.data)) == 0, nrow(train.data) > 0)

  if (!is.null(test.data)) assertthat::assert_that(sum(is.na(test.data)) == 0)

  assertthat::assert_that(sum(!is.element(colnames(train.data), ap.nms)) == 00)

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
