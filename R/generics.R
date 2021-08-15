#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_bar geom_text theme_minimal position_fill
#' @importFrom cowplot plot_grid
#' @importFrom scales percent
#' @export
autoplot.fairadapt <- function(x, when = "after", ...) {

  ..x.. <- ..count.. <- NULL

  plt <- list()

  for (tgt in c("train", "adapt.train")) {

    whn <- switch(tgt, train = "before", adapt.train = "after")

    vals <- x[[tgt]][[1L]]
    protected <- x[["train"]][[x$prot.attr]]
    df <- data.frame(vals, protected)

    if (length(unique(vals)) > 2L) {

      plt[[tgt]] <- ggplot(df, aes(x = vals, fill = protected)) +
        geom_density(alpha = 0.4) +
        scale_fill_discrete(name = x$prot.attr) +
        ggtitle(paste("Densities", whn , "adaptation")) +
        xlab(names(x$train)[1L]) +
        theme_minimal()

    } else {

      plt[[tgt]] <- ggplot(df, aes(x = protected, fill = factor(vals))) +
        geom_bar(position = "fill") +
        geom_text(
          aes(
            label = percent(
              round(..count.. / tapply(..count.., ..x.. , sum)[..x..], 4)
            )
          ), stat = "count", position = position_fill(0.5)
        ) +
        scale_y_continuous(labels = percent) +
        scale_fill_discrete(name = names(x$train)[1L]) +
        ggtitle(paste("Outcome proportions", whn, "adaptation")) +
        xlab(x$prot.attr) +
        theme_minimal()
    }
  }

  switch(when,
    before = plt[["train"]],
    after = plt[["adapt.train"]],
    plot_grid(plotlist = plt, ncol = 2L)
  )
}

#' @importFrom graphics lines plot polygon
#' @export
print.fairadapt <- function(x, ...) {

  cat("Fairadapt result\n\n")
  cat("Call:\n", deparse(x$formula), "\n\n")
  cat("Protected attribute:                 ", x$prot.attr, "\n")
  cat("Protected attribute levels:          ",
      paste(sort(unique(x$train[[x$prot.attr]])), collapse = ", "), "\n")

  if(!is.null(x$res.vars)) {
    cat("Resolving variables:                 ",
        paste(x$res.vars, collapse = ", "), "\n")
  }

  cat("Number of training samples:          ", nrow(x$adapt.train), "\n")
  cat("Number of test samples:              ", nrow(x$adapt.test), "\n")
  cat("Number of independent variables:     ", ncol(x$adapt.train) - 1L, "\n")

  seq_row <- seq_len(nrow(x$train))

  cat("Total variation (before adaptation): ",
    mean(x$train[[1L]][x$base.ind[seq_row]]) -
      mean(x$train[[1L]][!x$base.ind[seq_row]]),
    "\n"
  )

  cat("Total variation (after adaptation):  ",
    mean(x$adapt.train[[1L]][x$base.ind[seq_row]]) -
      mean(x$adapt.train[[1L]][!x$base.ind[seq_row]]),
    "\n"
  )

  invisible(x)
}

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
#' @param x Object of class \code{fairadapt}, a result of an adaptation
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
#' @param x Object of class \code{fairadapt}, a result of an adaptation
#' procedure.
#' @param train A logical indicating whether train data should be returned.
#' Defaults to \code{TRUE}. If \code{FALSE}, test data is returned.
#' @export
adaptedData <- function(x, train = TRUE) {
  UseMethod("adaptedData", x)
}

#' @export
adaptedData.fairadapt <- function(x, train = TRUE) {

  if (train) x[["adapt.train"]] else x[["adapt.test"]]

}

#' Fair Twin Inspection convenience function.
#'
#' @param x Object of class \code{fairadapt}, a result of an adaptation
#' procedure.
#' @param train.id A vector of indices specifying which rows of the training
#' data should be displayed.
#' @param test.id A vector of indices specifying which rows of the test
#' data should be displayed.
#' @param cols A \code{character} vector, subset of \code{names(train.data)},
#' which specifies which subset of columns is to be displayed in the result.
#' @return A \code{data.frame}, containing the original and adapted values
#' of the requested individuals. Adapted columns have \code{_adapted} appended
#' to their original name.
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
#' fairTwins(FA, train.id = 1:5)
#' @export
fairTwins <- function(x, train.id = 1L, test.id = NULL, cols = NULL) {
  UseMethod("fairTwins", x)
}

#' @export
fairTwins.fairadapt <- function(x, train.id = 1L, test.id = NULL,
                                cols = NULL) {

  if (!is.null(cols) && !is.element(x$prot.attr, cols)) {
    cols <- c(x$prot.attr, cols)
  }

  if (!is.null(train.id)) {

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

#' @export
predict.fairadapt <- function(object, newdata, ...) {

  assert_that(all(names(object$adapt.test) %in% names(newdata)),
              msg = "Columns missing in newdata")

  engine <- object$q.engine

  newdata[, object$prot.attr] <- relevel(
    as.factor(newdata[, object$prot.attr]),
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
        all(newdata[, var] %in% engine[[var]][["unique.values"]]),
        msg = paste0("New, unseen values of variable ", var, ". Disallowed.")
      )

      newdata[, var] <- factor(newdata[, var],
                               levels = engine[[var]][["unique.values"]])

      newdata[, var] <- as.integer(newdata[, var]) +
        runif(length(newdata[, var]), -0.5, 0.5)

      adapt[, var] <- newdata[, var]
    }

    # ii) computeQuants()
    adapt[!base.ind, var] <-
      computeQuants(
        engine[[var]][["object"]],
        newdata[, c(var, engine[[var]][["parents"]]), drop = FALSE],
        adapt[!base.ind, engine[[var]][["parents"]], drop = FALSE],
        base.ind, test = TRUE
      )

    # iii) decode discrete
    if (engine[[var]][["discrete"]]) {

      newdata[, var] <-
        decodeDiscrete(newdata[, var], engine[[var]][["unique.values"]],
                       engine[[var]][["type"]], length(newdata[, var]))
      adapt[, var] <-
        decodeDiscrete(adapt[, var], engine[[var]][["unique.values"]],
                       engine[[var]][["type"]], length(adapt[, var]))
    }
  }

  adapt
}
