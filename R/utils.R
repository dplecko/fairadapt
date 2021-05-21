
swap <- function(x, i, j) {

  keep <- x[i]
  x[i] <- x[j]
  x[j] <- keep

  x
}

seq_row <- function(x) seq_len(nrow(x))

seq_col <- function(x) seq_len(ncol(x))

makeLength <- function(x, train.len, full.len) {

  assert_that(length(x) %in% c(train.len, full.len), !is.factor(x))

  if (length(x) != full.len) {
    x <- c(x, rep(NA, full.len - train.len))
  }

  x
}

catOrder <- function(outcome, var) {

  u.val <- levels(var)
  u.val <- u.val[(u.val %in% var)]

  cat.enc <- rep(0, length(u.val))
  cond.expect <- cat.enc

  for (i in seq_along(u.val)) {

    cond.expect[i] <- mean(as.numeric(outcome[var == u.val[i]]), na.rm = TRUE)

    assert_that(!is.na(cond.expect[i]),
                msg = paste("New factor levels appearing in the test data",
                            "which is disallowed"))
  }

  if (length(cond.expect) != length(unique(cond.expect))) {
    cond.expect <- cond.expect + rnorm(
      length(cond.expect), sd = sd(cond.expect) / 10000
    )
  }

  u.val[order(cond.expect)]
}

marginalMatching <- function(x, base.ind) {

  x.baseline <- round(x[base.ind])
  x.non.baseline <- x[!base.ind]

  fitted.value.counts <- (tabulate(x.baseline) / length(x.baseline)) *
    length(x.non.baseline)
  fitted.x.baseline <- NULL

  for (i in seq_along(fitted.value.counts)) {

    fitted.x.baseline <- c(
      fitted.x.baseline,
      rep(i, round(fitted.value.counts[i]))
    )
  }

  fitted.x.baseline <- c(
    fitted.x.baseline,
    rep(
      length(fitted.value.counts),
      max(0, length(x.non.baseline) - length(fitted.x.baseline))
    )
  )

  fitted.x.baseline <- fitted.x.baseline[seq_along(x.non.baseline)]

  x.non.baseline[order(x.non.baseline)] <- fitted.x.baseline

  x[!base.ind] <- x.non.baseline
  x[base.ind] <- x.baseline

  x
}

decodeDiscrete <- function(var, u.val, type, full.len) {

  if (length(var) < full.len) {
    var <- c(var, rep(NA_real_, full.len - length(var)))
  }

  indices <- as.integer(round(var))

  assert_that(all(indices[!is.na(indices)] > 0L),
              all(indices[!is.na(indices)] <= length(u.val)),
              msg = "New value appearing, unseen in train data")

  decode <- u.val[indices]

  assert_that(
    is.element(type, c("numeric", "integer", "factor", "character")),
    msg = "Unexpected class in decoding"
  )

  switch(type,
    integer = as.integer(decode),
    factor = factor(decode, levels = u.val),
    numeric = as.numeric(decode),
    character = decode
  )
}
