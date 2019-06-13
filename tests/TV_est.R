TV_estimate <- function(p1, p2, nsplits = 5000) {
  grid <- (1:nsplits)/nsplits
  cdf1 <- sapply(1:nsplits, function(i) sum(p1 <= grid[i])/length(p1))
  cdf2 <- sapply(1:nsplits, function(i) sum(p2 <= grid[i])/length(p2))

  tv.dist <- 0
  for (i in 1:nsplits) {
    for (j in 1:nsplits) {
      tv.dist <- max(tv.dist, abs(abs(cdf1[i] - cdf1[j]) - abs(cdf2[i] - cdf2[j])))
    }
  }
  return(tv.dist)
}

p1 <- runif(4000)
p2 <- 1/2 + runif(50000, 0, 1/2)

TV_estimate(p1, p2, nsplits = 1000)

summarise <- function(Y.probs, Y.true, baseline.indicator) {
  return(c(as.numeric(pROC::auc(response = Y.true, predictor = Y.probs)), TV_estimate(Y.probs[baseline.indicator],
    Y.probs[!baseline.indicator])))
}
