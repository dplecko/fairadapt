#' @importFrom ggplot2 autoplot ggplot aes geom_density ggtitle
#' @importFrom ggplot2 scale_fill_discrete xlab scale_y_continuous
#' @importFrom ggplot2 geom_bar geom_text theme_minimal
#' @importFrom cowplot plot_grid
#' @method autoplot fairadapt
#' @export
autoplot.fairadapt <- function(x, when = "after", ...) {

  ..x.. <- ..count.. <- NULL

  plt <- list()

  for(tgt in c("train", "adapt.train")) {

    whn <- ifelse(tgt == "train", "before", "after")

    vals <- x[[tgt]][[1L]]
    protected <- x[["train"]][[x$protect.A]]
    df <- data.frame(vals, protected)

    if (length(unique(vals)) > 2L) {

      plt[[tgt]] <- ggplot(df, aes(x = vals, fill = protected)) +
        geom_density(alpha = 0.4) + theme_minimal() +
        ggtitle(paste("Densities", whn , "adaptation")) +
        scale_fill_discrete(name = x$protect.A) + xlab(names(x$train)[1L])

    } else {

      get_pos <- function(cnt) c(
          (cnt[2] + cnt[1]/2) / (cnt[1]+cnt[2]),
          cnt[2]/2 / (cnt[1]+cnt[2]),
          (cnt[4] + cnt[3]/2) / (cnt[3]+cnt[4]),
          cnt[4]/2 / (cnt[3]+cnt[4])
      )

      plt[[tgt]] <- ggplot(df, aes(x = protected, fill = factor(vals))) +
        geom_bar(position = "fill") + theme_minimal() +
        xlab(x$protect.A) + ggtitle(paste("Outcome proportions", whn,
                                          "adaptation")) +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_discrete(name = names(x$train)[1L]) +
        geom_text(aes( y= get_pos(..count..),
          label = scales::percent(
            round(..count../tapply(..count.., ..x.. ,sum)[..x..], 4))
          ),
          stat="count", vjust=0)

    }

  }

  if (when == "before") return(plt[["train"]])
  if (when == "after") return(plt[["adapt.train"]])

  return(cowplot::plot_grid(plotlist = plt, ncol = 2L))

}

#' @importFrom graphics lines plot polygon
#' @method print fairadapt
#' @export
print.fairadapt <- function(x, ...) {

  cat("Fairadapt result\n\n")
  cat("Call:\n", deparse(x$formula), "\n\n")
  cat("Protected attribute:                 ", x$protect.A, "\n")
  cat("Protected attribute levels:          ",
      paste(sort(unique(x$train[[x$protect.A]])), collapse = ", "), "\n")
  if(!is.null(x$res.vars)) cat("Resolving variables:                 ",
                               paste(x$res.vars, collapse = ", "), "\n")
  cat("Number of training samples:          ", nrow(x$adapt.train), "\n")
  cat("Number of test samples:              ", nrow(x$adapt.test), "\n")
  cat("Number of independent variables:     ", ncol(x$adapt.train) - 1L,
    "\n")

  cat("Total variation (before adaptation): ",
    mean(x$train[[1L]][x$base.ind[1:nrow(x$train)]]) -
      mean(x$train[[1L]][!x$base.ind[1:nrow(x$train)]]),
    "\n")
  cat("Total variation (after adaptation):  ",
    mean(x$adapt.train[[1L]][x$base.ind[1:nrow(x$train)]]) -
      mean(x$adapt.train[[1L]][!x$base.ind[1:nrow(x$train)]]),
    "\n")

}

#' @method plot fairadapt
#' @export
plot.fairadapt <- function(x, graph = F, when = "after", ...) {

  if (graph) {

    plot(x$graph, ...)

  } else if (length(unique(x$train[[1L]])) > 2L){

    base.idx <- x$base.ind[1:nrow(x$train)]
    if (when == "before") {

      target <- "train"

    } else if (when == "after") {

      target <- "adapt.train"

    } else target <- c("train", "adapt.train")

    for (tgt in target) {

      base.val <- x[[tgt]][[1L]][base.idx]
      nonbase.val <- x[[tgt]][[1L]][!base.idx]
      base.dens <- density(base.val)
      nonbase.dens <- density(nonbase.val)
      plot(base.dens,
        main = paste("Densities", ifelse(tgt == "train", "before", "after"),
                     "adaptation"),
        col = "blue", ylim = c(0, max(max(base.dens$y), max(nonbase.dens$y))),
        xlab = names(x$train)[1L])
      lines(nonbase.dens, col = "red")
      polygon(base.dens, col = "steelblue", density = 50)
      polygon(nonbase.dens, col = "red", density = 50)

    }
  } else {

    autoplot(x, when = when, ...)

  }

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
#'   adj.mat = uni.adj.mat, protect.A = "gender")
#'
#' fairTwins(FA, train.id = 1:5)
#' @export
fairTwins <- function(x, train.id = 1L, test.id = NULL, cols = NULL) {
  UseMethod("fairTwins", x)
}

#' @method fairTwins fairadapt
#' @export
fairTwins.fairadapt <- function(x, train.id = 1L, test.id = NULL, cols = NULL) {

  if (!is.null(cols) & !is.element(x$protect.A, cols))
    cols <- c(x$protect.A, cols)

  if (!is.null(train.id)) {

    target.id <- train.id
    if (is.null(cols)) cols <- names(x$train)
    df.target <- x$train[, cols, drop = F]
    df.adapt <- x$adapt.train[train.id, cols, drop = F]
    names(df.adapt) <- paste0(names(df.adapt), "_adapted")

  } else if (!is.null(test.id)) {

    target.id <- test.id
    if (is.null(cols)) cols <- names(x$adapt.test)
    df.target <- x$test[, cols, drop = F]
    df.adapt <- x$adapt.test[test.id, cols, drop = F]
    names(df.adapt) <- paste0(names(df.adapt), "_adapted")

  }

  col.ord <- sapply(setdiff(names(df.target), x$protect.A),
    function(lab) paste0(lab, c("", "_adapted")))
  col.ord <- c(x$protect.A, col.ord)
  res <- cbind(df.target[target.id, ,drop = F], df.adapt)

  res[, col.ord]

}

#' @method predict fairadapt
#' @export
predict.fairadapt <- function(object, newdata, ...) {

  assertthat::assert_that(all(names(object$adapt.test) %in% names(newdata)),
                          msg = "Columns missing in newdata")

  engine <- object$q.Engine
  newdata[, object$protect.A] <- as.factor(newdata[, object$protect.A])
  newdata[, object$protect.A] <- relevel(newdata[, object$protect.A],
                                         ref = object$base.lvl)
  adapt <- newdata
  adapt[, object$protect.A] <- factor(object$base.lvl,
                                      levels = levels(newdata[, object$protect.A]))
  base.ind <- newdata[, object$protect.A] == object$base.lvl

  for (var in setdiff(names(engine), all.vars(object$formual)[1L])) {


    assertthat::assert_that(class(newdata[, var]) == engine[[var]][["type"]],
                            msg = "Mismatch in column type with training data")

    # i) encode the variable if needed
    if (engine[[var]][["discrete"]]) {

      assertthat::assert_that(
        all(newdata[, var] %in% engine[[var]][["unique.values"]]),
        msg = paste0("New, unseen values of variable ", var, ". Disallowed.")
      )

      newdata[, var] <- factor(newdata[, var],
                               levels = engine[[var]][["unique.values"]])

      int.enc <- as.integer(newdata[, var]) +
                 runif(length(newdata[, var]), -0.5, 0.5)

      newdata[, var] <- adapt[, var] <- int.enc

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

      adapt.var <-
        DecodeDiscrete(adapt[, var], engine[[var]][["unique.values"]],
                       engine[[var]][["type"]], length(adapt[, var]))

      newdata.var <-
        DecodeDiscrete(newdata[, var], engine[[var]][["unique.values"]],
                       engine[[var]][["type"]], length(newdata[, var]))

      newdata[, var] <- newdata.var
      adapt[, var] <- adapt.var

    }

  }

  return(adapt)

}
