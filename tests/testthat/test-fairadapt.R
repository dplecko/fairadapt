
test_that("fairadapt", {

  with_seed(201, {
    train <- data_gen(100)
    test  <- data_gen(100)
  })

  vars <- c("a", "y", "x")

  expect_setequal(colnames(train), vars)
  expect_setequal(colnames(test), vars)

  adj.mat <- c(
    0L, 0L, 1L, # a
    0L, 0L, 0L, # y
    0L, 1L, 0L  # x
  )

  adj.mat <- matrix(adj.mat, nrow = length(vars), ncol = length(vars),
                    byrow = TRUE, dimnames = list(vars, vars))

  fa.nms <- c("adapt.train", "adapt.test", "train", "test", "base.lvl",
              "attr.lvls", "base.ind", "formula", "res.vars", "prot.attr",
              "graph", "quant.method", "adapt.call", "adj.mat", "cfd.mat",
              "top.ord", "q.engine")

  # random forest

  ran <- with_seed(202,
    fairadapt(y ~ ., train.data = train, test.data = test, adj.mat = adj.mat,
              prot.attr = "a", seed = 202)
  )

  # both print() and str() throw

  expect_type(ran, "list")
  expect_named(ran, fa.nms, ignore.order = TRUE)

  expect_s3_class(ran, "fairadapt")
  expect_s3_class(ran[["adapt.train"]], "data.frame")
  expect_s3_class(ran[["adapt.test"]], "data.frame")

  expect_identical(ran[["prot.attr"]], "a")

  expect_snapshot_json(tot_var(ran, "train", "y"))
  expect_snapshot_json(tot_var(ran, "adapt.train", "y"))

  ran.eng <- ran[["q.engine"]]

  expect_type(ran.eng, "list")
  expect_named(ran.eng, setdiff(vars, "a"), ignore.order = TRUE)

  for (i in setdiff(vars, "a")) {

    expect_true("object" %in% names(ran.eng[[i]]))

    obj <- ran.eng[[i]][["object"]]

    expect_s3_class(obj, "rangersplit")

    expect_named(obj, c("class0", "class1"))
    expect_s3_class(obj[["class0"]], "ranger")
    expect_s3_class(obj[["class1"]], "ranger")

    expect_true("parents" %in% names(ran.eng[[i]]))

    expect_identical(
      ran.eng[[i]][["parents"]],
      names(which(adj.mat[, i] == 1L))
    )
  }

  # quantFit()
  expect_error(quantFit(ran), regexp = "eval.qfit")

  # linear

  lin <- with_seed(202,
    fairadapt(y ~ ., train.data = train, test.data = test, adj.mat = adj.mat,
              prot.attr = "a", quant.method = linearQuants)
  )

  # both print() and str() throw

  expect_type(lin, "list")
  expect_named(lin, fa.nms, ignore.order = TRUE)

  expect_s3_class(lin, "fairadapt")
  expect_s3_class(lin[["adapt.train"]], "data.frame")
  expect_s3_class(lin[["adapt.test"]], "data.frame")

  expect_identical(lin[["prot.attr"]], "a")

  expect_snapshot_json(tot_var(lin, "train", "y"))
  expect_snapshot_json(tot_var(lin, "adapt.train", "y"))

  lin.eng <- lin[["q.engine"]]

  expect_type(lin.eng, "list")
  expect_named(lin.eng, setdiff(vars, "a"), ignore.order = TRUE)

  for (i in setdiff(vars, "a")) {

    expect_true("object" %in% names(lin.eng[[i]]))

    obj <- lin.eng[[i]][["object"]]

    expect_s3_class(obj, "quantregsplit")

    expect_named(obj, c("class0", "class1"))
    expect_s3_class(obj[["class0"]], "rqs")
    expect_s3_class(obj[["class1"]], "rqs")

    expect_true("parents" %in% names(lin.eng[[i]]))

    expect_identical(
      lin.eng[[i]][["parents"]],
      names(which(adj.mat[, i] == 1L))
    )
  }

  # neural network

  qrn <- with_seed(202,
    fairadapt(y ~ ., train.data = train, test.data = test, adj.mat = adj.mat,
              prot.attr = "a", quant.method = mcqrnnQuants)
  )

  expect_type(qrn, "list")
  expect_named(qrn, fa.nms, ignore.order = TRUE)

  expect_s3_class(qrn, "fairadapt")
  expect_s3_class(qrn[["adapt.train"]], "data.frame")
  expect_s3_class(qrn[["adapt.test"]], "data.frame")

  expect_identical(qrn[["prot.attr"]], "a")

  expect_snapshot_json(tot_var(qrn, "train", "y"))
  expect_snapshot_json(tot_var(qrn, "adapt.train", "y"))

  qrn.eng <- qrn[["q.engine"]]

  expect_type(qrn.eng, "list")
  expect_named(qrn.eng, setdiff(vars, "a"), ignore.order = TRUE)

  for (i in setdiff(vars, "a")) {

    expect_true("object" %in% names(qrn.eng[[i]]))

    obj <- qrn.eng[[i]][["object"]]

    expect_s3_class(obj, "mcqrnnobj")

    expect_true("parents" %in% names(qrn.eng[[i]]))

    expect_identical(
      qrn.eng[[i]][["parents"]],
      names(which(adj.mat[, i] == 1L))
    )
  }

  # w/ top.ord

  rto <- with_seed(202,
    fairadapt(y ~ ., train.data = train, test.data = test,
              top.ord = c("a", "x", "y"), prot.attr = "a", seed = 202)
  )

  expect_type(rto, "list")
  expect_named(rto, fa.nms, ignore.order = TRUE)
  expect_s3_class(rto, "fairadapt")
  rto.sum <- summary(rto)
  expect_s3_class(rto.sum, "summary.fairadapt")
  expect_output(print(rto.sum), regexp = "Call:")

  # for (i in setdiff(vars, "a")) {
  #   expect_identical(
  #     rto[["q.engine"]][[i]][["parents"]],
  #     ran[["q.engine"]][[i]][["parents"]]
  #   )
  # }

  skip_on_cran()

  # character example
  uni <- uni_admission
  uni$test <- ifelse(uni$test > 0, "A", "B")
  adj.mat <- c(
    0L, 1L, 1L, 1L, # gender
    0L, 0L, 0L, 1L, # edu
    0L, 0L, 0L, 1L, # test
    0L, 0L, 0L, 0L # score
  )

  adj.mat <- matrix(adj.mat, nrow = length(names(uni)), ncol = length(names(uni)),
                    byrow = TRUE, dimnames = list(names(uni), names(uni)))

  charmod <- with_seed(
    203,
    fairadapt(score ~ ., train.data = uni, adj.mat = adj.mat,
              prot.attr = "gender", seed = 203, eval.qfit = 3L)
  )

  expect_true(is.character(adaptedData(charmod)$test))

  # data example

  data <- system.file("testdata", "compas-scores-two-years.rds",
                      package = "fairadapt")
  data <- readRDS(data)

  cols <- c("age", "sex", "juv_fel_count", "juv_misd_count", "juv_other_count",
            "priors_count","c_charge_degree", "race", "two_year_recid")

  adj.mat <- c(
    0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, # age
    0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, # sex
    0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, # juv_fel_count
    0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, # juv_misd_count
    0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, # juv_other_count
    0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, # priors_count
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, # c_charge_degree
    0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, # race
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L  # two_year_recid
  )

  adj.mat <- matrix(adj.mat, nrow = length(cols), ncol = length(cols),
                    byrow = TRUE, dimnames = rep(list(cols), 2L))

  train <- head(data, n = nrow(data) / 2)
  test  <- tail(data, n = nrow(data) / 2)

  mod <- with_seed(203,
    fairadapt(two_year_recid ~ ., train.data = train, test.data = test,
              adj.mat = adj.mat, prot.attr = "race", seed = 203)
  )

  expect_output(print(mod), regexp = "Call:")
  mod.sum <- summary(mod)
  expect_s3_class(mod.sum, "summary.fairadapt")
  expect_output(print(mod.sum), regexp = "Call:")

  ind <- train[["race"]] == "White"

  expect_equal(mod[["adapt.train"]][["priors_count"]][ind],
                              train[["priors_count"]][ind])

  expect_gt(10, mean((mod[["adapt.train"]][["priors_count"]][!ind] -
                                     train[["priors_count"]][!ind]) ^ 2))

  for (i in names(mod[["q.engine"]])) {
    expect_identical(
      mod[["q.engine"]][[i]][["parents"]],
      names(which(adj.mat[, i] == 1L))
    )
  }

  # synthetic example

  funs <- list(
    x1 = function(a, x, eps) {
      2 * a - 1 + eps
    },
    x2 = function(a, x, eps) {
      2 * a - 1 + 1 / 25 * (x[[1]] + 5) ^ 2 + eps
    },
    x3 = function(a, x, eps) {
      1 / 4 * (x[[1]] + 5) * (x[[2]] + 6) + eps
    },
    x4 = function(a, x, eps) {
      1 * x[[2]] * log(x[[2]] + 50) + 1 / 50 * x[[3]] ^ 3 + eps
    }
  )

  vars <- c("a", names(funs))
  n    <- 2000

  with_seed(204, {
    a   <- rbinom(n, 1, 0.5)
    eps <- mvtnorm::rmvnorm(n, mean = rep(0, length(funs)))
  })

  train <- sem(funs, a, eps)
  test  <- tail(train, n = n / 2)
  train <- head(train, n = n / 2)

  cfac <- sem(funs, rep(0, n), eps)
  cfac <- tail(cfac, n = n / 2)

  adj.mat <- c(
    0L, 1L, 1L, 0L, 0L, # a
    0L, 0L, 1L, 1L, 0L, # x1
    0L, 0L, 0L, 1L, 1L, # x2
    0L, 0L, 0L, 0L, 1L, # x3
    0L, 0L, 0L, 0L, 0L  # x4
  )

  adj.mat <- matrix(adj.mat, nrow = length(vars), ncol = length(vars),
                    byrow = TRUE, dimnames = rep(list(vars), 2L))
  cfd.mat <- diag(ncol(adj.mat))
  dimnames(cfd.mat) <- dimnames(adj.mat)

  form <- as.formula(paste(tail(names(funs), n = 1), "~", "."))
  idx  <- test$a == 1

  mse.rf <- vector("list", length(funs) - 1L)
  names(mse.rf) <- head(names(funs), n = length(funs) - 1L)

  # random forest

  fair.sep <- with_seed(205,
    fairadapt(form, train.data = train, test.data = NULL, adj.mat = adj.mat,
              cfd.mat = cfd.mat, prot.attr = "a")
  )

  fair.sep <- with_seed(205, predict(fair.sep, newdata = test))

  fair.join <- with_seed(205,
    fairadapt(form, train.data = train, test.data = test, adj.mat = adj.mat,
              cfd.mat = cfd.mat, prot.attr = "a")
  )

  fair.join <- fair.join[["adapt.test"]]

  for (i in names(mse.rf)) {
    mse.rf[[i]] <- list(
      signif(100 * mean((fair.join[idx, i] - cfac[idx, i]) ^ 2)),
      signif(100 * mean((fair.sep[idx, i]  - cfac[idx, i]) ^ 2))
    )
  }

  expect_lt(mse.rf[["x1"]][[1L]], 5)
  expect_lt(mse.rf[["x1"]][[2L]], 5)

  expect_lt(mse.rf[["x2"]][[1L]], 10)
  expect_lt(mse.rf[["x2"]][[2L]], 10)

  expect_lt(mse.rf[["x3"]][[1L]], 100)
  expect_lt(mse.rf[["x3"]][[2L]], 100)

  expect_snapshot_json(mse.rf)

  # linear

  mse.lin <- vector("list", length(funs) - 1L)
  names(mse.lin) <- head(names(funs), n = length(funs) - 1L)

  fair.sep <- with_seed(205,
    fairadapt(form, train.data = train, test.data = NULL, adj.mat = adj.mat,
              cfd.mat = cfd.mat, prot.attr = "a", quant.method = linearQuants)
  )

  fair.sep <- with_seed(205, predict(fair.sep, newdata = test))

  fair.join <- with_seed(205,
    fairadapt(form, train.data = train, test.data = test, adj.mat = adj.mat,
              cfd.mat = cfd.mat, prot.attr = "a",  quant.method = linearQuants)
  )

  fair.join <- fair.join[["adapt.test"]]

  for (i in names(mse.lin)) {
    mse.lin[[i]] <- list(
      signif(100 * mean((fair.join[idx, i] - cfac[idx, i]) ^ 2)),
      signif(100 * mean((fair.sep[idx, i]  - cfac[idx, i]) ^ 2))
    )
  }

  expect_lt(mse.lin[["x1"]][[1L]], 5)
  expect_lt(mse.lin[["x1"]][[2L]], 5)

  expect_lt(mse.lin[["x2"]][[1L]], 10)
  expect_lt(mse.lin[["x2"]][[2L]], 10)

  expect_lt(mse.lin[["x3"]][[1L]], 100)
  expect_lt(mse.lin[["x3"]][[2L]], 100)

  expect_snapshot_json(mse.lin, tolerance = 0.2)
})
