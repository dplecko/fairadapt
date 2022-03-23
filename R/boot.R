#' Fairadapt Boostrap wrapper
#' 
#' The wrapper function for performing bootstrap uncertainty quantification for
#' the `fairadapt()` function.
#'
#' @param formula Object of class `formula` describing the response and
#' the covariates.
#' @param prot.attr A value of class `character` describing the binary
#' protected attribute. Must be one of the entries of `colnames(adj.mat)`.
#' @param adj.mat Matrix of class `matrix` encoding the relationships in
#' the causal graph. `M[i,j] == 1L` implies the existence of an edge from
#' node i to node j. Must include all the variables appearing in the formula
#' object. When the `adj.mat` argument is set to `NULL`, then the
#' `top.ord` argument has to be supplied.
#' @param train.data,test.data Training data & testing data, both of class
#' `data.frame`. Test data is by default `NULL`.
#' @param cfd.mat Symmetric matrix of class `matrix` encoding the
#' bidirected edges in the causal graph. `M[i,j] == M[j, i] == 1L`
#' implies the existence of a bidirected edge between nodes i and j. Must
#' include all the variables appearing in the formula object.
#' @param top.ord A vector of class `character` describing the
#' topological ordering of the causal graph. Default value is `NULL`,
#' but this argument must be supplied if `adj.mat` is not specified.
#' Also must include all the variables appearing in the formula object.
#' @param res.vars A vector of class `character` listing all the resolving
#' variables, which should not be changed by the adaption procedure. Default
#' value is `NULL`, corresponding to no resolving variables. Resolving
#' variables should be a subset of the descendants of the protected attribute.
#' @param quant.method A function choosing the method used for quantile
#' regression. Default value is `rangerQuants` (using random forest
#' quantile regression). Other implemented options are `linearQuants` and
#' `mcqrnnQuants`. A custom function can be supplied by the user here,
#' and the associated method for the S3 generic `computeQuants` needs to be
#' added.
#' @param keep.object a `logical` scalar, indicating whether all the
#' `fairadapt` S3 objects built in bootstrap repetitions should be saved.
#' @param n.boot An integer corresponding to the umber of bootstrap iterations.
#' @param rand.mode A string, taking values `"finsamp"`, `"quant"` or `"both"`,
#' corresponding to considering finite sample uncertainty, quantile
#' uncertainty, or both.
#' @param ... Additional arguments forwarded to the function passed as
#' `quant.method`.
#'
#' @return An object of class `fairadaptBoot`, containing the original and
#' adapted training and testing data, together with the causal graph and some
#' additional meta-information.
#' @examples
#' n_samp <- 200
#' uni_dim <- c(       "gender", "edu", "test", "score")
#' uni_adj <- matrix(c(       0,     1,      1,       0,
#'                            0,     0,      1,       1,
#'                            0,     0,      0,       1,
#'                            0,     0,      0,       0),
#'                   ncol = length(uni_dim),
#'                   dimnames = rep(list(uni_dim), 2),
#'                   byrow = TRUE)
#'
#' uni_ada <- fairadaptBoot(score ~ .,
#'   train.data = head(uni_admission, n = n_samp),
#'   test.data = tail(uni_admission, n = n_samp),
#'   adj.mat = uni_adj,
#'   prot.attr = "gender",
#'   n.boot = 10
#' )
#'
#' uni_ada
#'
#' @references
#' Plecko, D. & Meinshausen, N. (2019).
#' Fair Data Adaptation with Quantile Preservation
#' @export
fairadaptBoot <- function(formula, prot.attr, adj.mat, train.data, 
                          test.data = NULL, cfd.mat = NULL, top.ord = NULL, 
                          res.vars = NULL, quant.method = rangerQuants,
                          keep.object = FALSE, n.boot = 100, 
                          rand.mode = c("finsamp", "quant", "both"),
                          ...) {

  rand.mode <- match.arg(rand.mode)
  
  trn.rnd <- rand.mode %in% c("finsamp", "both")
  tst.rnd <- rand.mode %in% c("quant", "both")
  
  res.lst <- list()
  FA.lst <- list()
  boot.lst <- list()

  if (!trn.rnd) {
    FA <- fairadapt(formula, prot.attr, adj.mat, train.data, 
                    cfd.mat = cfd.mat, top.ord = top.ord, 
                    res.vars = res.vars, quant.method = quant.method, ...)
  }
  
  if (missing(adj.mat)) {
    adj.mat <- NULL
  }

  for (rep in seq_len(n.boot)) {
    # retrain if needed
    boot.smp <- seq_len(nrow(train.data))

    if (trn.rnd) {
      
      bad <- TRUE
      cnt <- 1L

      if (!is.null(test.data)) {
        while (any(bad)) {
          boot.smp <- sample(nrow(train.data), replace = TRUE)
          bad <- vapply(
            which(vapply(train.data, function(x) is.factor(x) | is.character(x),
                         logical(1L))),
            function(i) {
              length(setdiff(test.data[, i], train.data[boot.smp, i])) > 0
            },
            logical(1L)
          )
        }
      }
      
      boot.lst[[rep]] <- boot.smp
      
      FA <- fairadapt(formula, prot.attr, adj.mat, train.data[boot.smp, ],
                      cfd.mat = cfd.mat, top.ord = top.ord, res.vars = res.vars,
                      quant.method = quant.method, ...)
      
      if (keep.object) {
        FA.lst[[rep]] <- FA
      }
    }
    
    # fix test randomness if needed
    if (!tst.rnd) {
      set.seed(2022)
    }

    if (!is.null(test.data)) {
      res.lst[[rep]] <- predict(FA, test.data)
    }
  }
  
  if (!keep.object) {
    FA.lst <- NULL
  }
  
  structure(
    list(
      rand.mode = rand.mode,
      n.boot = n.boot,
      keep.object = keep.object,
      prot.attr = prot.attr,
      adj.mat = adj.mat,
      res.vars = res.vars,
      cfd.mat = cfd.mat,
      top.ord = top.ord,
      adapt.test = res.lst,
      boot.ind = boot.lst,
      fairadapt = FA.lst,
      boot.call = match.call(),
      formula = formula,
      last.mod = FA
    ),
    class = "fairadaptBoot"
  )
}

#' @export
print.fairadaptBoot <- function(x, ...) {
  
  cat("\nCall:\n", paste(deparse(x$boot.call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  
  cat("Bootstrap repetitions:", x$n.boot, "\n")

  if (!is.null(x$adj.mat)) {
    vars <- setdiff(getDescendants(x$prot.attr, x$adj.mat), x$res.vars)
  } else {
    attr.idx <- which(x$top.ord == x$prot.attr)
    if (attr.idx < length(x$top.ord)) {
      vars <- x$top.ord[seq.int(attr.idx + 1L, length(x$top.ord))]
    } 
  }
  cat("\nAdapting variables:\n  ", paste0(vars, collapse = ", "), "\n",
      sep = "")
  
  cat("\nBased on protected attribute", x$prot.attr, "\n")
  
  cat("\n  AND\n")
  
  if (is.null(x$adj.mat)) {

    cat("\nBased on topological order:\n  ", x$top.ord, sep = "")

  } else {

    mat <- rbind(colnames(x$adj.mat), x$adj.mat)
    mat <- apply(mat, 2L, format, justify = "right")
    mat <- cbind(format(c("", rownames(x$adj.mat))), mat)

    cat("\nBased on causal graph:\n")
    cat(apply(mat, 1L, paste, collapse = " "), sep = "\n")
    cat("\n")
  }
  
  invisible(x)
}

#' @export
summary.fairadaptBoot <- function(object, ...) {

  # FIXME: determine from top.ord?
  if (is.null(object$adj.mat)) {
    adapt.vars <- NULL
  } else {
    adapt.vars <- setdiff(
      getDescendants(object$prot.attr, object$adj.mat),
      object$res.vars
    )
  }

  mod <- object$last.mod
  
  structure(
    list(
      boot.call = object$boot.call,
      prot.attr = object$prot.attr,
      attr.lvls = mod$attr.lvls,
      res.vars = object$res.vars,
      train.samp = nrow(mod$adapt.train),
      test.samp = nrow(mod$adapt.test),
      adapt.vars = adapt.vars,
      n.boot = object$n.boot,
      keep.object = object$keep.object,
      rand.mode = object$rand.mode,
      quant.method = mod$quant.method
    ),
    class = "summary.fairadaptBoot"
  )
}

#' @export
print.summary.fairadaptBoot <- function(x, ...) {
    
  cat("\nCall:\n", paste(deparse(x$boot.call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
    
  cat("Bootstrap repetitions:     ", x$n.boot, "\n")
    
  cat("Protected attribute:       ", x$prot.attr, "\n")
  cat("Protected attribute levels:",
      paste(sort(x$attr.lvls), collapse = ", "), "\n")
    
  if (!is.null(x$adapt.vars)) {
    cat("Adapted variables:         ",
        paste(x$adapt.vars, collapse = ", "), "\n")
  }

  if(!is.null(x$res.vars)) {
    cat("Resolving variables:       ",
        paste(x$res.vars, collapse = ", "), "\n")
  }

  cat("\n")

  cat("Number of training samples:", x$train.samp, "\n")
  cat("Number of test samples:    ", x$test.samp, "\n")
  cat("Quantile method:           ", x$quant.method, "\n")

  cat("\n")

  cat("Randomness considered:     ", x$rand.mode, "\n")
  cat("fairadapt objects saved:   ", x$keep.object, "\n")

  invisible(x)
}

#' @export
predict.fairadaptBoot <- function(object, newdata, ...) {
  
  assert_that(!is.null(object$fairadapt),
              msg = paste("Object cannot be used for making new predictions.",
                          "Need to run `fairadaptBoot()` with `keep.object`",
                          "= TRUE to be able to do so."))
  
  assert_that(all(names(object$fairadapt[[1]]$adapt.train[[1]]) %in% 
                  names(newdata)),
              msg = "Columns missing in newdata")
  
  lapply(object$fairadapt, predict, newdata = newdata)
}

#' @export
adaptedData.fairadaptBoot <- function(x, train = TRUE) {
  if (isTRUE(train)) x[["adapt.train"]] else x[["adapt.test"]]
}
