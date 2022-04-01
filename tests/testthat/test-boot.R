
test_that("fairadaptBoot", {
  
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
  
  fa.nms <- c("rand.mode", "n.boot", "keep.object", "prot.attr", "adj.mat",
              "res.vars", "cfd.mat", "top.ord", "adapt.test", "boot.ind",
              "fairadapt", "boot.call", "formula", "last.mod")
  
  # random forest
  expect_message(
    with_seed(202,
              fairadaptBoot(y ~ ., train.data = train, test.data = test, 
                            adj.mat = adj.mat, prot.attr = "a", seed = 202,
                            n.boot = 3L, keep.object = TRUE, test.seed = 202)
    ), regexp = "^A non-default value for the `seed` argument is ignored"
  )
  
  ran <- with_seed(202,
                   fairadaptBoot(y ~ ., train.data = train, test.data = test, 
                                 adj.mat = adj.mat, prot.attr = "a", seed = 202,
                                 n.boot = 3L, keep.object = TRUE)
  )

  expect_type(ran, "list")
  expect_named(ran, fa.nms, ignore.order = TRUE)
  
  expect_s3_class(ran, "fairadaptBoot")
  expect_s3_class(ran[["adapt.test"]][[1]], "data.frame")
  expect_s3_class(ran[["adapt.test"]][[2]], "data.frame")
  
  adda <- adaptedData(ran, train = TRUE)
  expect_type(adda, "list")
  expect_s3_class(adda[[1]], "data.frame")
  
  adda <- adaptedData(ran, train = FALSE)
  expect_type(adda, "list")
  expect_s3_class(adda[[1]], "data.frame")
  
  expect_identical(ran[["prot.attr"]], "a")
  
  expect_snapshot_json(tot_var(ran$last.mod, "train", "y"))
  expect_snapshot_json(tot_var(ran$last.mod, "adapt.train", "y"))
  
  ran.eng <- ran[["fairadapt"]][[1]][["q.engine"]]
  
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

  expect_snapshot(print(ran))
  expect_snapshot(summary(ran))

  # w/ top.ord
  
  rto <- with_seed(202,
                   fairadaptBoot(y ~ ., train.data = train, test.data = test,
                                top.ord = c("a", "x", "y"), prot.attr = "a", 
                                seed = 202, n.boot = 3L)
  )
  
  expect_type(rto, "list")
  expect_named(rto, fa.nms, ignore.order = TRUE)
  expect_s3_class(rto, "fairadaptBoot")

  expect_snapshot(print(rto))
  expect_snapshot(summary(rto))

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
  
  adj.mat <- matrix(adj.mat, nrow = length(names(uni)),
                    ncol = length(names(uni)), byrow = TRUE,
                    dimnames = list(names(uni), names(uni)))
  
  charmod <- with_seed(
    203,
    fairadaptBoot(score ~ ., train.data = uni, adj.mat = adj.mat,
                  prot.attr = "gender", seed = 203, n.boot = 3L,
                  keep.object = TRUE)
  )
  
  charmod.pred <- predict(charmod, uni)
  expect_type(charmod.pred, "list")

  expect_snapshot(print(charmod))
  expect_snapshot(summary(charmod))

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
                   fairadaptBoot(two_year_recid ~ ., train.data = train, 
                                 test.data = test, adj.mat = adj.mat, 
                                 prot.attr = "race", seed = 203,
                                 n.boot = 3)
  )

  expect_snapshot(print(mod))
  expect_snapshot(summary(mod))
  
  expect_error(
    adaptedData(mod), 
    regexp = "Adapted training data not available when `keep.object` = FALSE"
  )
  
  adap <- adaptedData(mod, train = FALSE)
  expect_type(adap, "list")
  expect_s3_class(adap[[1]], "data.frame")
  
})
