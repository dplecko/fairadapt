#' Fairadapt Boostrap wrapper
#' 
#' The wrapper function for performing bootstrap uncertainty quantification for
#' the `fairadapt()` function.
#'
#' @param formula Object of class \code{formula} describing the response and
#' the covariates.
#' @param prot.attr A value of class \code{character} describing the binary
#' protected attribute. Must be one of the entries of \code{colnames(adj.mat)}.
#' @param adj.mat Matrix of class \code{matrix} encoding the relationships in
#' the causal graph. \code{M[i,j] == 1L} implies the existence of an edge from
#' node i to node j. Must include all the variables appearing in the formula
#' object. When the \code{adj.mat} argument is set to \code{NULL}, then the
#' \code{top.ord} argument has to be supplied.
#' @param train.data,test.data Training data & testing data, both of class
#' \code{data.frame}. Test data is by default \code{NULL}.
#' @param cfd.mat Symmetric matrix of class \code{matrix} encoding the
#' bidirected edges in the causal graph. \code{M[i,j] == M[j, i] == 1L}
#' implies the existence of a bidirected edge between nodes i and j. Must
#' include all the variables appearing in the formula object.
#' @param top.ord A vector of class \code{character} describing the
#' topological ordering of the causal graph. Default value is \code{NULL},
#' but this argument must be supplied if \code{adj.mat} is not specified.
#' Also must include all the variables appearing in the formula object.
#' @param res.vars A vector of class \code{character} listing all the resolving
#' variables, which should not be changed by the adaption procedure. Default
#' value is \code{NULL}, corresponding to no resolving variables. Resolving
#' variables should be a subset of the descendants of the protected attribute.
#' @param quant.method A function choosing the method used for quantile
#' regression. Default value is \code{rangerQuants} (using random forest
#' quantile regression). Other implemented options are \code{linearQuants} and
#' \code{mcqrnnQuants}. A custom function can be supplied by the user here,
#' and the associated method for the S3 generic \code{computeQuants} needs to be
#' added.
#' @param ... Additional arguments forwarded to the function passed as
#' `quant.method`.
#'
#' @return An object of class \code{fairadaptBoot}, containing the original and
#' adapted training and testing data, together with the causal graph and some
#' additional meta-information.
#' @examples
#' uni.adj.mat <- array(0, dim = c(4, 4))
#' colnames(uni.adj.mat) <- rownames(uni.adj.mat) <-
#'   c("gender", "edu", "test", "score")
#'
#' uni.adj.mat["gender", c("edu", "test")] <-
#'   uni.adj.mat["edu", c("test", "score")] <-
#'   uni.adj.mat["test", "score"] <- 1L
#'
#' FA <- fairadapt(score ~ .,
#'   train.data = uni_admission[1:100, ],
#'   test.data = uni_admission[101:150, ],
#'   adj.mat = uni.adj.mat, prot.attr = "gender")
#'
#' FA
#'
#' @author Drago Plecko
#' @references
#' Plecko, D. & Meinshausen, N. (2019).
#' Fair Data Adaptation with Quantile Preservation \cr
#' @import stats
#' @importFrom assertthat assert_that
#' @export
fairadaptBoot <- function(formula, prot.attr, adj.mat, train.data, 
                          test.data = NULL, cfd.mat = NULL, top.ord = NULL, 
                          res.vars = NULL, quant.method = rangerQuants,
                          save.object = FALSE, n.boot = 100, 
                          rand.mode = "finsamp", 
                          ...) {
  
  trn.rnd <- ifelse(rand.mode %in% c("finsamp", "both"), TRUE, FALSE)
  tst.rnd <- ifelse(rand.mode %in% c("quant", "both"), TRUE, FALSE)
  
  res.lst <- FA.lst <- bot.lst <- list() 
  
  if (!trn.rnd) {
    FA <- fairadapt(two_year_recid ~ ., "race", adj.mat, train.data,
                    cfd.mat = cfd.mat, top.ord = top.ord, res.vars = res.vars,
                    quant.method = quant.method, ...)
  }

  for (rep in seq_len(n.boot)) {
    # retrain if needed
    bot.smp <- seq_len(nrow(train.data))
    if (trn.rnd) {
      
      bad <- TRUE
      cnt <- 1L
      while (any(bad)) {
        cnt <- cnt + 1L
        bot.smp <- sample(nrow(train.data), replace = TRUE)
        bad <- vapply(
          seq_len(ncol(train.data)),
          function(i) {
            length(setdiff(test.data[, i], train.data[bot.smp, i])) > 0
          }, logical(1L)
        )
      }
      
      bot.lst[[rep]] <- bot.smp
      
      FA <- fairadapt(two_year_recid ~ ., "race", adj.mat, train.data[bot.smp, ],
                      cfd.mat = cfd.mat, top.ord = top.ord, res.vars = res.vars,
                      quant.method = quant.method, ...)
      
      if (save.object) FA.lst[[rep]] <- FA
    }
    
    # fix test randomness if needed
    if (!tst.rnd) set.seed(2022)
    res.lst[[rep]] <- predict(FA, test.data)
    
  }
  
  if (!save.object) FA.lst <- NULL
  
  structure(list(
    mode = mode,
    n.boot = n.boot,
    adapt.test = res.lst,
    boot.ind = bot.lst,
    fairadapt = FA.lst
  ), class = "fairadaptBoot")
  
}

#' @export
predict.fairadaptBoot <- function(object, newdata, ...) {
  
  assert_that(!is.null(object$fairadapt),
              msg = paste("Object cannot be used for making new predictions.",
                          "Need to run `fairadaptBoot()` with `save.object`",
                          "= TRUE to be able to do so."))
  
  assert_that(all(names(object$adapt.test[[1]]) %in% names(newdata)),
              msg = "Columns missing in newdata")
  
  lapply(object$fairadapt, predict, newdata = newdata)
  
}
