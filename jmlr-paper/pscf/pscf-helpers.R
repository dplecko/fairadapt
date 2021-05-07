auc_gap <- function(out, prob, attr) {
  return(
    c(mean(as.integer(prob >= 0.5) == out), #PRROC::roc.curve(scores.class0 = prob, weights.class0 = out)$auc,
      abs(mean(prob[attr == 1] >= 0.5) - mean(prob[attr == 0] >= 0.5)))
  )
}

p_df <- function(L_prob, attr, out) {
  n <- length(L_prob[[1]])
  
  res <- NULL
  print(sapply(L_prob, function(x) {
    mean(as.integer(x >= 0.5) == out)
  }))
  for(i in 1:20) {
    bts <- sample(1:n, round(0.8*n))
    res <- rbind(res, as.vector(sapply(L_prob, function(x) {
      auc_gap(out[bts], x[bts], attr[bts])
    })))
    
  }
  
  df <- NULL
  for(i in 1:(ncol(res)/2)) {
    auc <- mean(res[, 2*i-1])
    gap <- mean(res[, 2*i])
    
    auc.min <- auc - sd(res[, 2*i-1])
    auc.max <- auc + sd(res[, 2*i-1])
    
    gap.min <- gap - sd(res[, 2*i])
    gap.max <- gap + sd(res[, 2*i])
    
    df <- rbind(
      df, c(auc, auc.min, auc.max, gap, gap.min, gap.max)
    )
  }
  df <- data.frame(df)
  names(df) <- c("auc", "auc.min", "auc.max", "gap", "gap.min", "gap.max")
  
  cbind(df, Method = names(L_prob))
}