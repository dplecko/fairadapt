
test_that("generics", {

  with_seed(301, {
    train <- data_gen(100, add_z = TRUE)
    test  <- data_gen(100, add_z = TRUE)
    pred  <- data_gen(100, add_z = TRUE)
  })

  vars <- c("a", "y", "x", "z")

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

  # random forest

  ad.rf <- with_seed(302,
    fairadapt(y ~ ., train.data = train, test.data = test, adj.mat = adj.mat,
              protect.A = "a", seed = 302)
  )

  aut.plt <- autoplot(ad.rf)

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_plot("auto_rf", print(aut.plt))
  # extra print should not be necessary
  expect_snapshot_plot("plot_rf", print(plot(ad.rf)))
  expect_snapshot_plot("graph_rf", with_seed(302, plot(ad.rf, graph = TRUE)))

  expect_snapshot_csv("ftdef_rf", fairTwins(ad.rf))
  expect_snapshot_csv("fttrn_rf", fairTwins(ad.rf, train.id = NULL,
                                            test.id = 1L))
  expect_snapshot_csv("predi_rf", with_seed(302, predict(ad.rf, pred)))

  # linear

  ad.lin <- with_seed(303,
    fairadapt(y ~ ., train.data = train, test.data = test, adj.mat = adj.mat,
              protect.A = "a", quant.method = linearQuants)
  )

  aut.plt <- autoplot(ad.lin)

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_plot("auto_lin", print(aut.plt))
  # extra print should not be necessary
  expect_snapshot_plot("plot_lin", print(plot(ad.lin)))
  expect_snapshot_plot("graph_lin", with_seed(303, plot(ad.lin, graph = TRUE)))

  expect_snapshot_csv("ftdef_lin", fairTwins(ad.lin))
  expect_snapshot_csv("fttrn_lin", fairTwins(ad.lin, train.id = NULL,
                                             test.id = 1L))
  expect_snapshot_csv("predi_lin", with_seed(303, predict(ad.lin, pred)))

  # cts

  cts <- with_seed(304, data_gen(100, add_z = TRUE))
  cts$Y <- cts$X

  ad.cts <- with_seed(304,
    fairadapt(y ~ ., train.data = cts, test.data = cts, adj.mat = adj.mat,
              protect.A = "a", seed = 304)
  )

  aut.plt <- autoplot(ad.cts)

  expect_s3_class(aut.plt, "ggplot")

  expect_snapshot_plot("auto_cts", print(aut.plt))
  # extra print should not be necessary
  expect_snapshot_plot("plot_cts", print(plot(ad.cts)))
  expect_snapshot_plot("graph_cts", with_seed(304, plot(ad.cts, graph = TRUE)))

  expect_snapshot_csv("ftdef_cts", fairTwins(ad.cts))
  expect_snapshot_csv("fttrn_cts", fairTwins(ad.cts, train.id = NULL,
                                             test.id = 1L))
  expect_snapshot_csv("predi_cts", with_seed(304, predict(ad.cts, pred)))
})
