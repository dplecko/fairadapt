
test_that("input verification", {

  with_seed(101, {
    train <- data_gen(100)
    test  <- data_gen(100)
  })

  vars <- c("a", "y", "x")

  expect_setequal(colnames(train), vars)
  expect_setequal(colnames(test), vars)

  adj.mat <- c(
  #  a,  y,  x
    0L, 0L, 1L, # a
    0L, 0L, 0L, # y
    0L, 1L, 0L  # x
  )

  adj.mat <- matrix(adj.mat, nrow = length(vars), ncol = length(vars),
                    byrow = TRUE, dimnames = list(vars, vars))

  expect_error(
    fairadapt(Y ~ z + x, train.data = train, test.data = test,
              adj.mat = adj.mat, protect.A = "a"),
    "object 'Y' not found"
  )

  expect_error(
    fairadapt(y ~ z + x, train.data = train, test.data = test,
              adj.mat = adj.mat, protect.A = "a"),
    "object 'z' not found"
  )

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = adj.mat, protect.A = "A"),
    "is not TRUE"
  )

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = adj.mat + 1, protect.A = "a"),
    "is not TRUE"
  )

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = adj.mat, protect.A = "a",
              quant.method = "noname"),
    "could not find function \"quant.method\""
  )

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = adj.mat, protect.A = "a",
              res.vars = "noname"),
    "not equal to 0"
  )

  na.dat   <- with_seed(102, data_gen(100))
  na.dat$x <- NA

  expect_error(
    fairadapt(y ~ ., train.data = na.dat, test.data = test,
              adj.mat = adj.mat, protect.A = "a"),
    "not greater than 0"
  )

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = na.dat,
              adj.mat = adj.mat, protect.A = "a"),
    "not equal to 0"
  )

  w.dat <- with_seed(103, data_gen(100))
  w.dat <- cbind(w.dat, z = 0)

  expect_error(
    fairadapt(y ~ ., train.data = w.dat, test.data = test,
              adj.mat = adj.mat, protect.A = "a"),
    "undefined columns selected"
  )

  tmp.mat <- adj.mat
  tmp.mat["y", "x"] <- 1L

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat, protect.A = "a"),
    "is not TRUE"
  )

  tmp.mat <- adj.mat
  dimnames(tmp.mat) <- NULL

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat, protect.A = "a"),
    "is not TRUE"
  )

  tmp.mat <- adj.mat
  rownames(tmp.mat) <- c("a", "a", "a")

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat, protect.A = "a"),
    "is not TRUE"
  )

  colnames(tmp.mat) <- c("a", "a", "a")

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat, protect.A = "a"),
    "is not TRUE"
  )

  tmp.mat <- adj.mat
  colnames(tmp.mat) <- c("a", "x2", "y")

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat, protect.A = "a"),
    "is not TRUE"
  )

  tmp.mat <- adj.mat
  rownames(tmp.mat) <- c("a", "x1", "y")

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat, protect.A = "a"),
    "is not TRUE"
  )

  tmp.mat <- adj.mat
  dimnames(tmp.mat) <- rep(list(c("a", "x1", "y")), 2)

  expect_error(
    fairadapt(y ~ ., train.data = train, test.data = test,
              adj.mat = tmp.mat[c("x1", "y"), c("x1", "y")], protect.A = "a"),
    "is not TRUE"
  )
})
