#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle after_stat
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_bar geom_text theme_minimal position_fill
#' @importFrom cowplot plot_grid
#' @importFrom scales percent
#' @export
autoplot.fairadapt <- function(object, when = "after", ...) {

  plt <- list()

  for (tgt in c("train", "adapt.train")) {

    whn <- switch(tgt, train = "before", adapt.train = "after")

    vals <- object[[tgt]][[1L]]
    protected <- object[["train"]][[object$prot.attr]]
    df <- data.frame(vals, protected)

    if (length(unique(vals)) > 2L) {

      plt[[tgt]] <- ggplot(df, aes(x = vals, fill = protected)) +
        geom_density(alpha = 0.4) +
        scale_fill_discrete(name = object$prot.attr) +
        ggtitle(paste("Densities", whn , "adaptation")) +
        xlab(names(object$train)[1L]) +
        theme_minimal()

    } else {

      plt[[tgt]] <- ggplot(df, aes(x = protected, fill = factor(vals))) +
        geom_bar(position = "fill") +
        geom_text(
          aes(
            label = percent(
              round(after_stat(count) / tapply(after_stat(count), after_stat(x) ,
                                               sum)[after_stat(x)], 4)
            )
          ), stat = "count", position = position_fill(0.5)
        ) +
        scale_y_continuous(labels = percent) +
        scale_fill_discrete(name = names(object$train)[1L]) +
        ggtitle(paste("Outcome proportions", whn, "adaptation")) +
        xlab(object$prot.attr) +
        theme_minimal()
    }
  }

  switch(when,
         before = plt[["train"]],
         after = plt[["adapt.train"]],
         plot_grid(plotlist = plt, ncol = 2L)
  )
}

#' @export
print.fairadapt <- function(x, ...) {

  cat("\nCall:\n", paste(deparse(x$adapt.call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  if (!is.null(x$adj.mat)) {
    vars <- setdiff(getDescendants(x$prot.attr, x$adj.mat), x$res.vars)
  } else {
    attr.idx <- which(x$top.ord == x$prot.attr)
    if (attr.idx < length(x$top.ord)) {
      vars <- x$top.ord[seq.int(attr.idx + 1L, length(x$top.ord))]
    } else {
      vars <- NULL
    }
  }

  if (!is.null(vars)) {
    cat("\nAdapting variables:\n  ", paste0(vars, collapse = ", "), "\n",
        sep = "")
  } else {
    cat("\nNo adapted variables\n")
  }

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
summary.fairadapt <- function(object, ...) {

  seq_row <- seq_len(nrow(object$train))

  tv.start <- mean(object$train[[1L]][object$base.ind[seq_row]]) -
    mean(object$train[[1L]][!object$base.ind[seq_row]])

  tv.end <- mean(object$adapt.train[[1L]][object$base.ind[seq_row]]) -
    mean(object$adapt.train[[1L]][!object$base.ind[seq_row]])

  # FIXME: determine from top.ord?
  adapt.vars <- setdiff(
    getDescendants(object$prot.attr, object$adj.mat),
    object$res.vars
  )

  structure(
    list(
      formula = object$formula,
      prot.attr = object$prot.attr,
      attr.lvls = object$attr.lvls,
      res.vars = object$res.vars,
      train.samp = nrow(object$adapt.train),
      test.samp = nrow(object$adapt.test),
      adapt.vars = adapt.vars,
      tv.start = tv.start,
      tv.end = tv.end,
      quant.method = object$quant.method
    ),
    class = "summary.fairadapt"
  )
}

#' @export
print.summary.fairadapt <- function(x,
                                    digits = max(3L, getOption("digits") - 3L),
                                    ...) {

  cat0("\nCall:\n", paste(deparse(x$adapt.call), sep = "\n", collapse = "\n"),
       "\n\n")

  cat0("Protected attribute:                 ", x$prot.attr, "\n")
  cat0("Protected attribute levels:          ",
       paste(sort(x$attr.lvls), collapse = ", "), "\n")

  if (!is.null(x$adapt.vars)) {
    cat0("Adapted variables:                   ",
         paste(x$adapt.vars, collapse = ", "), "\n")
  }

  if(!is.null(x$res.vars)) {
    cat0("Resolving variables:                 ",
         paste(x$res.vars, collapse = ", "), "\n")
  }

  cat0("\n")

  cat0("Number of training samples:          ", x$train.samp, "\n")
  cat0("Number of test samples:              ", x$test.samp, "\n")
  cat0("Quantile method:                     ", x$quant.method, "\n")

  cat0("\n")

  cat0("Total variation (before adaptation): ",
       format(x$tv.start, digits = digits), "\n")

  cat0("Total variation (after adaptation):  ",
       format(x$tv.end, digits = digits), "\n")

  invisible(x)
}

#' @importFrom graphics lines plot polygon
#' @importFrom graphics barplot text
#' @export
plot.fairadapt <- function(x, when = "after", ...) {

  base.idx <- x$base.ind[seq_len(nrow(x$train))]

  target <- c("train", "adapt.train")
  target <- switch(when, before = target[1L], after = target[2L], target)
  reg <- length(unique(x$train[[1L]])) > 2L

  for (tgt in target) {

    base.val <- x[[tgt]][[1L]][base.idx]
    nonbase.val <- x[[tgt]][[1L]][!base.idx]

    if (reg) {

      base.dens <- density(base.val)
      nonbase.dens <- density(nonbase.val)

      plot(base.dens,
           main = paste("Densities", switch(tgt, train = "before", "after"),
                        "adaptation"),
           col = "blue",
           ylim = c(0, max(base.dens$y, nonbase.dens$y)),
           xlab = names(x$train)[1L]
      )

      lines(nonbase.dens, col = "red")
      polygon(base.dens, col = "steelblue", density = 50)
      polygon(nonbase.dens, col = "red", density = 50)

    } else {

      base.bar <- table(base.val)
      base.bar <- 100 * base.bar / sum(base.bar)

      nonbase.bar <- table(nonbase.val)
      nonbase.bar <- 100 * nonbase.bar / sum(nonbase.bar)

      x.names <- levels(x$train[[x$prot.attr]])

      barplot(cbind(base.bar, nonbase.bar),
              ylab = "Proportion", xlab = x$prot.attr,
              main = paste("Class balance",
                           switch(tgt, train = "before", "after"), "adaptation"),
              legend = TRUE, args.legend = list(title = names(x$train)[1L]),
              names.arg = x.names, col = c("red", "steelblue"), density = 50

      )
      # first text
      val.f <- c(base.bar[1L], nonbase.bar[1L])
      text(val.f / 2L, labels = paste0(round(val.f, 2), "%"))

      # second text
      val.s <- c(base.bar[2L], nonbase.bar[2L])
      text(val.f + val.s / 2L, labels = paste0(round(val.s, 2), "%"))

    }

  }

}

#' Visualize Graphical Causal Model
#'
#' @param x Object of class `fairadapt`, a result of an adaptation
#' procedure.
#' @param ... Additional arguments passed to the graph plotting function.
#' @export
visualizeGraph <- function(x, ...) {
  UseMethod("visualizeGraph", x)
}

#' @export
visualizeGraph.fairadapt <- function(x, ...) plot(x$graph, ...)

#' Convenience function for returning adapted data
#'
#' @param x Object of class `fairadapt` or `fairadaptBoot`, a result of an
#' adaptation procedure.
#' @param train A logical indicating whether train data should be returned.
#' Defaults to `TRUE`. If `FALSE`, test data is returned.
#' @return Either a `data.frame` when called on an `fairadapt` object, or a `list`
#' of `data.frame`s with the adapted data of length `n.boot`, when called on a
#' `fairadaptBoot` object.
#'
#' @export
adaptedData <- function(x, train = TRUE) {
  UseMethod("adaptedData", x)
}

#' @rdname adaptedData
#' @export
adaptedData.fairadapt <- function(x, train = TRUE) {

  if (train) x[["adapt.train"]] else x[["adapt.test"]]
}

#' @rdname adaptedData
#' @export
adaptedData.fairadaptBoot <- function(x, train = TRUE) {

  if (train && !x$keep.object) {
    stop("Adapted training data not available when `keep.object` = FALSE.")
  } else if (train && x$keep.object) {
    lapply(x$fairadapt, `[[`, "adapt.train")
  } else x[["adapt.test"]]
}

#' Fair Twin Inspection convenience function.
#'
#' @param x Object of class `fairadapt`, a result of an adaptation
#' procedure.
#' @param train.id A vector of indices specifying which rows of the training
#' data should be displayed.
#' @param test.id A vector of indices specifying which rows of the test
#' data should be displayed.
#' @param cols A `character` vector, subset of `names(train.data)`,
#' which specifies which subset of columns is to be displayed in the result.
#' @return A `data.frame`, containing the original and adapted values
#' of the requested individuals. Adapted columns have `_adapted` appended
#' to their original name.
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
#' uni_ada <- fairadapt(score ~ .,
#'   train.data = head(uni_admission, n = n_samp),
#'   test.data = tail(uni_admission, n = n_samp),
#'   adj.mat = uni_adj,
#'   prot.attr = "gender"
#' )
#'
#' fairTwins(uni_ada, train.id = 1:5)
#' @export
fairTwins <- function(x, train.id = seq_len(nrow(x$train)), test.id = NULL,
                      cols = NULL) {
  UseMethod("fairTwins", x)
}

#' @export
fairTwins.fairadapt <- function(x, train.id = seq_len(nrow(x$train)),
                                test.id = NULL, cols = NULL) {

  if (!is.null(cols) && !is.element(x$prot.attr, cols)) {
    cols <- c(x$prot.attr, cols)
  }

  if (!is.null(train.id)) {

    if (!is.null(test.id)) {
      cat(
        "Both `train.id` and `test.id` specified. Using `train.id` argument.\n"
      )
    }

    target.id <- train.id

    if (is.null(cols)) {
      cols <- names(x$train)
    }

    df.target <- x$train[, cols, drop = FALSE]
    df.adapt <- x$adapt.train[train.id, cols, drop = FALSE]

    names(df.adapt) <- paste0(names(df.adapt), "_adapted")

  } else if (!is.null(test.id)) {

    target.id <- test.id

    if (is.null(cols)) {
      cols <- names(x$adapt.test)
    }

    df.target <- x$test[, cols, drop = FALSE]
    df.adapt <- x$adapt.test[test.id, cols, drop = FALSE]

    names(df.adapt) <- paste0(names(df.adapt), "_adapted")
  }

  col.ord <- setdiff(names(df.target), x$prot.attr)
  col.ord <- rbind(col.ord, paste0(col.ord, "_adapted"))
  col.ord <- c(x$prot.attr, col.ord)

  res <- cbind(df.target[target.id, , drop = FALSE], df.adapt)

  res[, col.ord]
}


#' Prediction function for new data from a saved `fairadapt` object.
#'
#' @details The `newdata` argument should be compatible with `adapt.test`
#' argument that was used when constructing the `fairadapt` object. In
#' particular, `newdata` should contain column names that appear in the `formula`
#' argument that was used when calling `fairadapt()` (apart from the outcome
#' variable on the LHS of the formula).
#'
#' @param object Object of class `fairadapt`, a result of an adaptation
#' procedure.
#' @param newdata A `data.frame` containing the new data.
#' @param ... Additional arguments forwarded to `computeQuants()`.
#' @return A `data.frame` containing the adapted version of the new data.
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
#' uni_ada <- fairadapt(score ~ .,
#'   train.data = head(uni_admission, n = n_samp),
#'   adj.mat = uni_adj,
#'   prot.attr = "gender"
#' )
#'
#' predict(object = uni_ada, newdata = tail(uni_admission, n = n_samp))
#' @export
predict.fairadapt <- function(object, newdata, ...) {

  assert_that(all(names(object$adapt.test) %in% names(newdata)),
              msg = "Columns missing in newdata")

  engine <- object$q.engine

  newdata[, object$prot.attr] <- relevel(
    factor(newdata[, object$prot.attr], levels = object$attr.lvls),
    ref = object$base.lvl
  )

  adapt <- newdata
  adapt[, object$prot.attr] <- factor(
    object$base.lvl,
    levels = levels(newdata[, object$prot.attr])
  )

  base.ind <- newdata[, object$prot.attr] == object$base.lvl

  for (var in setdiff(names(engine), all.vars(object$formula)[1L])) {

    assert_that(class(newdata[, var]) == engine[[var]][["type"]],
                msg = "Mismatch in column type with training data")

    # i) encode the variable if needed
    if (engine[[var]][["discrete"]]) {

      assert_that(
        all(newdata[, var] %in% engine[[var]][["unique.values"]]) |
          is.integer(engine[[var]][["discrete"]]),
        msg = paste0("New, unseen values of variable ", var, ". Disallowed.")
      )

      if (is.logical(engine[[var]][["discrete"]])) {
        newdata[, var] <- factor(newdata[, var],
                                 levels = engine[[var]][["unique.values"]])
      }

      newdata[, var] <- as.integer(newdata[, var]) +
        runif(length(newdata[, var]), -0.5, 0.5)

      adapt[, var] <- newdata[, var]
    }

    # ii) computeQuants()
    if (sum(!base.ind) > 0L) {
      adapt[!base.ind, var] <-
        computeQuants(
          engine[[var]][["object"]],
          newdata[, c(var, engine[[var]][["parents"]]), drop = FALSE],
          adapt[!base.ind, engine[[var]][["parents"]], drop = FALSE],
          base.ind, test = TRUE, ...
        )
    }

    # iii) decode discrete
    if (engine[[var]][["discrete"]]) {

      if (is.integer(engine[[var]][["discrete"]])) {
        newdata[, var] <- as.integer(round(newdata[, var]))
        adapt[, var] <- as.integer(round(adapt[, var]))
      } else {
        newdata[, var] <-
          decodeDiscrete(newdata[, var], engine[[var]][["unique.values"]],
                         engine[[var]][["type"]], length(newdata[, var]))
        adapt[, var] <-
          decodeDiscrete(adapt[, var], engine[[var]][["unique.values"]],
                         engine[[var]][["type"]], length(adapt[, var]))
      }

    }
  }

  adapt
}

#' Quality of quantile fit statistics.
#'
#' @param x Object of class `fairadapt`, a result of an adaptation
#' procedure.
#' @param ... Ignored in this case.
#' @return A `numeric` vector, containing the average empirical loss for
#' the 25%, 50% and 75% quantile loss functions, for each variable.
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
#' uni_ada <- fairadapt(score ~ .,
#'   train.data = head(uni_admission, n = n_samp),
#'   test.data = tail(uni_admission, n = n_samp),
#'   adj.mat = uni_adj,
#'   prot.attr = "gender",
#'   eval.qfit = 3L
#' )
#'
#' quantFit(uni_ada)
#' @export
quantFit <- function(x, ...) {
  UseMethod("quantFit", x)
}

#' @export
quantFit.fairadapt <- function(x, ...) {

  qfit <- lapply(x$q.engine, `[[`, "qfit.score")

  assert_that(
    !is.null(qfit[[1L]]),
    msg = paste(
      "Run `fairadapt()` with `eval.qfit` equal to a positive integer",
      "to inspect quality of the fit."
    )
  )

  vapply(x$q.engine, `[[`, numeric(1L), "qfit.score")
}
