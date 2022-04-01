
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
  
  # round the baseline values
  x.base <- round(x[base.ind])
  x.nbas <- x[!base.ind]
  # compute the proportions
  fit.val.cnt <- (table(x.base) / length(x.base)) *
    length(x.nbas)
  u.val <- sort(unique(x.base))
  
  assert_that(length(u.val) == length(fit.val.cnt),
              msg = "Wrong number of unique values in Marginal Matching")
  
  fit.x.base <- NULL

  for (i in seq_along(fit.val.cnt)) {
    fit.x.base <- c(
      fit.x.base,
      rep(u.val[i], round(fit.val.cnt[i]))
    )
  }

  fit.x.base <- c(
    fit.x.base,
    rep(
      u.val[length(fit.val.cnt)],
      max(0, length(x.nbas) - length(fit.x.base))
    )
  )

  fit.x.base <- fit.x.base[seq_along(x.nbas)]

  x.nbas[order(x.nbas)] <- fit.x.base

  x[!base.ind] <- x.nbas
  x[base.ind] <- x.base

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

qfitScore <- function(x, quants, probs = c(0.25, 0.5, 0.75)) {
  
  tauLoss <- function(x, tau) ifelse(x > 0, tau * x, -(1-tau) * x)  
  
  qtarg <- lapply(seq_along(x), function(row) quantile(quants[row, ], 
                                                       probs = probs))
  qtarg <- Reduce(rbind, qtarg)
  
  mean(
    vapply(seq_col(qtarg), 
           function(col) mean(tauLoss(x - qtarg[, col], probs[col])), 
           numeric(1L))
  )
  
}

release_questions <- function() {
  paste("Is environment variable `FAIRADAPT_VIGNETTE_QUICK_BUILD` not set",
        "to `false`?")
}

cat0 <- function(...) cat(..., sep = "")
