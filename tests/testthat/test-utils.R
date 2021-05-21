
test_that("categorical encoding", {

  n <- 10000
  p <- c(0.5, 0.2, 0.1, 0.9, 0.3)

  with_seed(401, {
    x <- sample(seq_len(5), n, replace = TRUE)
    y <- rbinom(n, 1, p[x])
  })

  df <- data.frame(y, x = factor(x))

  mmatch1 <- marginalMatching(
    c(rep(c(1, 2, 3), each = 10), rep(0, 30)),
    rep(c(TRUE, FALSE), each = 30)
  )

  mmatch2 <- c(rep(c(1, 2, 3), each = 10), rep(c(1, 2, 3), each = 10))

  expect_equal(mmatch1, mmatch2)
})
