
test_that("generics", {

  # set.seed(2021) gives "New value appearing, unseen in train data"
  set.seed(2012)

  train <- dataGen(100, add_z = TRUE)
  test  <- dataGen(100, add_z = TRUE)
  pred  <- dataGen(100, add_z = TRUE)
  vars  <- c("a", "y", "x", "z")

  expect_setequal(colnames(train), vars)
  expect_setequal(colnames(test), vars)
  expect_setequal(colnames(pred), vars)

  adj.mat <- c(
    0L, 0L, 1L, 0L, # a
    0L, 0L, 0L, 0L, # y
    0L, 1L, 0L, 0L, # x
    1L, 1L, 1L, 0L  # z
  )

  adj.mat <- matrix(adj.mat, nrow = length(vars), ncol = length(vars),
                    byrow = TRUE, dimnames = list(vars, vars))

  funs <- c(rangerQuants, linearQuants)

  for (i in seq_along(funs)) {

    mod <- fairadapt(y ~ ., train.data = train, test.data = test,
                     adj.mat = adj.mat, protect.A = "a",
                     quant.method = funs[[i]])

    aut.plt <- autoplot(mod)

    expect_s3_class(aut.plt, "ggplot")

    expect_snapshot_file(save_png(print(aut.plt)),
                         paste0("auto_", i, ".png"))
    # extra print should not be necessary
    expect_snapshot_file(save_png(print(plot(mod))),
                         paste0("plot_", i, ".png"))
    expect_snapshot_file(save_png(plot(mod, graph = TRUE)),
                         paste0("graph_", i, ".png"))

    expect_snapshot_file(
      save_csv(fairTwins(mod)),
      paste0("ftdef_", i, ".csv")
    )

    expect_snapshot_file(
      save_csv(fairTwins(mod, train.id = NULL, test.id = 1L)),
      paste0("fttrn_", i, ".csv")
    )

    expect_snapshot_file(
      save_csv(predict(mod, pred)),
      paste0("predi_", i, ".csv")
    )
  }

  cts <- dataGen(100, add_z = TRUE)
  cts$Y <- cts$X

  i <- length(funs) + 1L

  mod <- fairadapt(y ~ ., train.data = cts, test.data = cts, adj.mat = adj.mat,
                    protect.A = "a")

  aut.plt <- autoplot(mod)

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_file(save_png(print(aut.plt)),
                       paste0("auto_", i, ".png"))
  # extra print should not be necessary
  expect_snapshot_file(save_png(print(plot(mod))),
                       paste0("plot_", i, ".png"))
  expect_snapshot_file(save_png(plot(mod, graph = TRUE)),
                       paste0("graph_", i, ".png"))

  expect_snapshot_file(
    save_csv(fairTwins(mod)),
    paste0("ftdef_", i, ".csv")
  )

  expect_snapshot_file(
    save_csv(fairTwins(mod, train.id = NULL, test.id = 1L)),
    paste0("fttrn_", i, ".csv")
  )

  expect_snapshot_file(
    save_csv(predict(mod, pred)),
    paste0("predi_", i, ".csv")
  )
})
