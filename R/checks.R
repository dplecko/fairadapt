
correctInput <- function(formula, train.data, test.data, adj.mat, cfd.mat,
  top.ord, protect.A, resolving.variables, quant.method) {

  if (is.null(adj.mat)) {

    assert_that(!is.null(top.ord))
    ap.nms <- top.ord

  } else {

    assert_that(
      !is.null(colnames(adj.mat)), !is.null(rownames(adj.mat)),
      withinRange(adj.mat),
      setequal(rownames(adj.mat), colnames(adj.mat))
    )

    adj.mat <- adj.mat[colnames(adj.mat), ]

    assert_that(isAcyclic(adj.mat))

    ap.nms <- colnames(adj.mat)
  }

  assert_that(
    length(ap.nms) == length(unique(ap.nms)),
    sum(!is.element(resolving.variables, colnames(adj.mat))) == 0,
    is.element(protect.A, ap.nms)
  )

  train.data <- model.frame(formula, train.data)

  if (!is.null(test.data)) {
    test.data <- test.data[, colnames(train.data)[-1]]
  }

  assert_that(sum(is.na(train.data)) == 0, nrow(train.data) > 0)

  if (!is.null(test.data)) {
    assert_that(sum(is.na(test.data)) == 0)
  }

  assert_that(sum(!is.element(colnames(train.data), ap.nms)) == 0)

  invisible(NULL)
}

withinRange <- function(mat) {
  sum(!is.element(mat, c(0, 1))) == 0
}

isAcyclic <- function(mat) {

  num.walks <- mat

  for (i in seq_len(nrow(mat))) {
    num.walks <- mat + num.walks %*% mat
  }

  sum(diag(num.walks)) == 0
}
