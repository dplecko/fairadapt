printf <- function(...) invisible(print(sprintf(...)))
expit <- function(x) exp(x) / (1+exp(x))
FairnessMetrics <- function(Y, Y.hat, baseline.indicator){
  pbr <- sum(Y.hat[baseline.indicator]) / sum(baseline.indicator)
  pnbr <- sum(Y.hat[!baseline.indicator]) / sum(!baseline.indicator)
  accuracy <- sum(Y == Y.hat, na.rm = TRUE) / (length(Y))
  #printf("accuracy %.2f %%", accuracy*100)
  #printf("-----------------------------")
  #printf("positive rate for baseline %.2f %%", 100*pbr)
  #printf("positive rate for non-baseline %.2f %%", 100*pnbr)
  #printf("parity gap %.2f %%", abs(pbr-pnbr)*100)
  #printf("-----------------------------")

  tp.b <- sum(Y.hat == 1 & Y == 1 & baseline.indicator)
  fn.b <- sum(Y.hat == 0 & Y == 1 & baseline.indicator)
  tn.b <- sum(Y.hat == 0 & Y == 0 & baseline.indicator)
  fp.b <- sum(Y.hat == 1 & Y == 0 & baseline.indicator)

  tp.nb <- sum(Y.hat == 1 & Y == 1 & !baseline.indicator)
  fn.nb <- sum(Y.hat == 0 & Y == 1 & !baseline.indicator)
  tn.nb <- sum(Y.hat == 0 & Y == 0 & !baseline.indicator)
  fp.nb <- sum(Y.hat == 1 & Y == 0 & !baseline.indicator)
 # printf("TPR/TNR for baseline (%.2f %%, %.2f %%)", 100*tp.b/(tp.b+fn.b), 100*tn.b/(tn.b+fp.b))
  #printf("TPR/TNR for non-base (%.2f %%, %.2f %%)", 100*tp.nb/(tp.nb+fn.nb), 100*tn.nb/(tn.nb+fp.nb))
  return(c(accuracy*100, 100*abs(pbr-pnbr)))
}

EqualisedOddsGenerator <- function(n) {
  A <- rbinom(n, size = 1, prob = 0.5)
  Y <- rbinom(n, size = 1, prob = 0.5 - (2 * A - 1) * 0.2 )
  X1 <- 2*Y - 0.5*A + rnorm(n)
  X2 <- Y - 0.3*A + rnorm(n)
  X3 <- -Y + 0.4*A + rnorm(n)
  df <- data.frame(cbind(Y,A,X1,X2,X3))
  colnames(df) <- c("Y","A","X1","X2","X3")
  return(df)
}

DemParityGenerator <- function(n) {
  A <- rbinom(n, size = 1, prob = 0.5)
  X1 <-  -3*A/4 + 3/8 + rnorm(n, sd = 2)
  X2 <- X1 + rnorm(n, sd = 0.1)
  #X3 <- -A + 1/2 + rnorm(n, sd = 2)
  Y <- rbinom(n, size = 1, prob = expit((X1 + X2)))
  df <- data.frame(cbind(Y, A, X1, X2))
  colnames(df) <- c("Y", "A", "X1", "X2")
  return(df)
}

ResolvingLevelGen <- function(n) {
  A <- rbinom(n, size = 1, prob = 0.5)
  X1 <-  -A/4 + 1/8 + rnorm(n, sd = 2)
  X2 <- -A/4 + 1/8 + rnorm(n, sd = 2)
  X3 <- -A/4 + 1/8 + rnorm(n, sd = 2)
  X4 <-  -A/4 + 1/8 + rnorm(n, sd = 2)
  X5 <-  -A/4 + 1/8 + rnorm(n, sd = 2)
  Y <- rbinom(n, size = 1, prob = expit((X1+X2+X3+X4+X5)))
  df <- data.frame(cbind(Y,A,X1,X2,X3,X4,X5))
  colnames(df) <- c("Y","A","X1","X2","X3","X4","X5")
  return(df)
}

RecodeLevels <- function(data) {
  Y.new <- rep("", nrow(data))
  A.new <- rep("", nrow(data))
  Y.new[ data[,"Y"] == 1] <- ">50K"
  Y.new[ data[,"Y"] == 0] <- "<=50K"
  A.new[ data[,"A"] == 1] <- "Female"
  A.new[ data[,"A"] == 0] <- "Male"
  data[, "Y"] <- Y.new
  data[, "A"] <- A.new
  return(data)
}
