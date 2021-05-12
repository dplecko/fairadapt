
test_that("graph utils", {

  cfd <- c(
    0L, 0L, 1L, 0L, 0L, # x1
    0L, 0L, 0L, 1L, 0L, # x2
    1L, 0L, 0L, 0L, 1L, # x3
    0L, 1L, 0L, 0L, 0L, # x4
    0L, 0L, 1L, 0L, 0L  # x5
  )

  var <- paste0("x", seq_len(sqrt(length(cfd))))
  cfd <- matrix(cfd, nrow = length(var), ncol = length(var),
                byrow = TRUE, dimnames = list(var, var))

  expect_equal(confoundedComponent("x1", cfd), c("x1", "x3", "x5"))

  adj <- c(
    0L, 1L, 0L, 0L, 0L, # x1
    0L, 0L, 1L, 0L, 0L, # x2
    0L, 0L, 0L, 1L, 0L, # x3
    0L, 0L, 0L, 0L, 1L, # x4
    0L, 0L, 0L, 0L, 0L  # x5
  )

  adj <- matrix(adj, nrow = length(var), ncol = length(var),
                byrow = TRUE, dimnames = list(var, var))

  expect_equal(adjustmentSet("x5", adj, cfd), c("x1", "x2", "x3", "x4"))

  adj <- c(
    0L, 1L, 0L, 0L, 0L, # a
    0L, 0L, 1L, 1L, 1L, # x2
    0L, 0L, 0L, 1L, 1L, # x3
    0L, 0L, 0L, 0L, 1L, # x4
    0L, 0L, 0L, 0L, 0L  # y
  )

  var <- c("a", "x1", "x2", "x3", "y")

  adj <- matrix(adj, nrow = length(var), ncol = length(var),
                byrow = TRUE, dimnames = list(var, var))
  cfd <- matrix(0L, nrow = length(var), ncol = length(var),
                dimnames = list(var, var))

  expect_false(nonId("a", adj, cfd))

  cfd[rbind(c("a", "x1"),
            c("x1", "a"))] <- 1L

  expect_true(nonId("a", adj, cfd))

  expect_equal(getDescendants("a", adj), c("x1", "x2", "x3", "y"))
  expect_equal(getParents("x2", adj), c("x1"))
  expect_equal(getAncestors("x2", adj), c("a", "x1"))
  expect_equal(topologicalOrdering(adj), c("a", "x1", "x2", "x3", "y"))
})
