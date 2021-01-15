test_that("Categorical Encoding Works", {

  n <- 10000
  X <- sample(1:5, n, replace = T)
  p <- c(0.5, 0.2, 0.1, 0.9, 0.3)
  Y <- rbinom(n, 1, p[X])
  df <- data.frame(Y, X = factor(X))

  ord1 <- CategoricalEncoding(Y, X, c(1, 2, 3, 4, 5))

  ord2 <- order(order(p))


  expect_equal(ord1, ord2)

  mmatch1 <- MarginalMatching(
    c(rep(c(1, 2, 3), each = 10), rep(0, 30)),
    rep(c(T, F), each = 30)
  )

  mmatch2 <- c(rep(c(1, 2, 3), each = 10), rep(c(1, 2, 3), each = 10))

  expect_equal(mmatch1, mmatch2)

  x <- c("a", "b", "c", "d", "a", "a", "b")

  L <- EncodeDiscrete(rbinom(length(x), 1, 0.5), factor(x))

  expect_equal(L[["unique.values"]], c("a", "b", "c", "d"))
  expect_equal(EncodeDiscrete(rbinom(length(x), 1, 0.5), x)[["cat.enc"]], c(1, 2, 3, 4))

})
