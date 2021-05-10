Swap <- function(x, i, j) {

  keep <- x[i]
  x[i] <- x[j]
  x[j] <- keep

  return(x)

}

expit <- function(x) return(exp(x)/(1+exp(x)))

MakeLength <- function(x, train.len, full.len) {

  assertthat::assert_that(length(x) %in% c(train.len, full.len))
  assertthat::assert_that(!is.factor(x))

  if(length(x) != full.len) x <- c(x, rep(NA, full.len - train.len))

  x

}
