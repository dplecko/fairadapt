test_that("Graph Utils Work", {

  cfd <- array(0, dim = c(5, 5))
  colnames(cfd) <- rownames(cfd) <- c("X1", "X2", "X3", "X4", "X5")
  cfd["X1", "X3"] <- cfd["X3", "X1"] <-  1
  cfd["X3", "X5"] <- cfd["X5", "X3"] <-  1
  cfd["X2", "X4"] <- cfd["X4", "X2"] <-  1

  adj <- array(0, dim = c(5, 5))
  colnames(adj) <- rownames(adj) <- c("X1", "X2", "X3", "X4", "X5")
  adj["X1", "X2"] <- adj["X2", "X3"] <- adj["X3", "X4"] <- adj["X4", "X5"] <- 1


  expect_equal(ConfoundedComponent("X1", cfd), c("X1", "X3", "X5"))

  expect_equal(AdjustmentSet("X5", adj, cfd), c("X1", "X2", "X3", "X4"))


  adj <- array(0, dim = c(5,5))
  colnames(adj) <- rownames(adj) <-  c("A", "X1", "X2", "X3", "Y")
  adj["A", "X1"] <- adj["X1", c("X2", "X3")] <- adj["X2", "X3"] <- 1
  adj[c("X1", "X2", "X3"), "Y"] <- 1

  cfd <- adj
  cfd[,] <- 0
  expect_false(NonID("A", adj, cfd))

  cfd["A", "X1"] <- cfd["X1", "A"] <- 1
  expect_true(NonID("A", adj, cfd))

  expect_equal(GetDescendants("A", adj), c("X1", "X2", "X3", "Y"))
  expect_equal(GetParents("X2", adj), c("X1"))
  expect_equal(GetAncestors("X2", adj), c("A", "X1"))
  expect_equal(TopologicalOrdering(adj), c("A", "X1", "X2", "X3", "Y"))

})
