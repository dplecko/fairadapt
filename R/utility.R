
CatOrder <- function(outcome, var, u.val) {

  u.val <- levels(var)
  cond.expect <- cat.enc <- rep(0, length(u.val))

  for (i in 1:length(u.val)) {

    cond.expect[i] <- mean(as.numeric(outcome[var == u.val[i]]), na.rm = TRUE)

    assertthat::assert_that(!is.na(cond.expect[i]),
                            msg = "New factor levels appearing in the test data, which is disallowed")

  }

  if (length(cond.expect) != length(unique(cond.expect)))
    cond.expect <- cond.expect +
    rnorm(length(cond.expect), sd = sd(cond.expect)/10000)

  u.val[order(cond.expect)]

}

MarginalMatching <- function(x, base.ind) {

  x.baseline <- round(x[base.ind])
  x.non.baseline <- x[!base.ind]

  fitted.value.counts <- (tabulate(x.baseline) / length(x.baseline)) *
    length(x.non.baseline)
  fitted.x.baseline <- NULL

  for (i in 1:length(fitted.value.counts)) {

    fitted.x.baseline <- c(fitted.x.baseline,
                           rep(i, round(fitted.value.counts[i])))

  }

  fitted.x.baseline <- c(fitted.x.baseline,
                         rep(length(fitted.value.counts),
                             max(0, length(x.non.baseline) - length(fitted.x.baseline))))
  fitted.x.baseline <- fitted.x.baseline[1:length(x.non.baseline)]

  x.non.baseline[order(x.non.baseline)] <- fitted.x.baseline
  x[!base.ind] <- x.non.baseline
  x[base.ind] <- x.baseline

  x

}

DecodeDiscrete <- function(var, u.val, type) {

  indices <- round(var)
  assertthat::assert_that(is.integer(indices), all(indices > 0L))

  decode <- u.val[indices]

  assertthat::assert_that(is.element(type, c("numeric", "integer", "factor",
                                             "character")),
                          msg = "Unexpected class in decoding")

  res <- switch(type, integer = as.integer(decode),
                factor = factor(decode, levels = u.val),
                numeric = as.numeric(decode),
                character = decode)

  res
}
