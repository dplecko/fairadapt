# Correctness tests for fairadaptMinBN() and fairadaptAgent().

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Return the adapted x1-vs-x0 mean gap for one variable.
adaptGap <- function(fit, dat, var, prot.attr = "x") {
  base <- dat[[prot.attr]] == levels(dat[[prot.attr]])[1L]
  mean(fit$adapt.train[!base, var]) - mean(dat[base, var])
}

# Fit either min-BN or max-BN using the same interface.
fitBN <- function(type, formula, dat, adj, B, prot.attr = "x", ...) {
  if (identical(type, "min")) {
    fairadaptMinBN(formula, prot.attr = prot.attr, adj.mat = adj,
                   train.data = dat, bn.set = B, ...)
  } else {
    fairadapt(formula, prot.attr = prot.attr, adj.mat = adj,
              train.data = dat, res.vars = B, ...)
  }
}

# Sum linear path coefficients retained by the requested BN criterion.
retainedSum <- function(B, type, coefs, nodes) {
  keep <- vapply(nodes, function(v) {
    if (identical(type, "min")) {
      all(v %in% B)
    } else {
      any(v %in% B)
    }
  }, logical(1L))
  
  sum(coefs[keep])
}

# Check forward closure among descendants of the protected attribute.
isFwdClosed <- function(B, adj, prot.attr = "x") {
  des <- fairadapt:::getDescendants(prot.attr, adj)
  
  all(vapply(B, function(v) {
    ch <- intersect(colnames(adj)[adj[v, ] != 0L], des)
    all(ch %in% B)
  }, logical(1L)))
}

# -----------------------------------------------------------------------------
# I. Exact implementation invariants on the package example
# -----------------------------------------------------------------------------

uni_dim <- c("gender", "edu", "test", "score")
uni_adj <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 1,
  0, 0, 0, 1,
  0, 0, 0, 0
), ncol = length(uni_dim), dimnames = rep(list(uni_dim), 2L), byrow = TRUE)

uni_dat <- head(uni_admission, n = 500L)

test_that("setWorld() changes only the target world", {
  obj <- structure(list(class0 = "zero", class1 = "one"), class = "rangersplit")
  
  expect_identical(setWorld(obj, FALSE), obj)
  
  out <- setWorld(obj, TRUE)
  expect_identical(out$class0, "one")
  expect_identical(out$class1, "one")
  expect_identical(obj$class0, "zero")
})

test_that("empty min-BN set reproduces fairadapt() exactly", {
  set.seed(2026)
  mx <- fairadapt(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                  prot.attr = "gender")
  
  set.seed(2026)
  mn <- fairadaptMinBN(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                       prot.attr = "gender", bn.set = NULL)
  
  expect_equal(mn$adapt.train, mx$adapt.train)
  expect_equal(mn$adapt.train, mn$adapt0.train)
})

test_that("adapt0 is the full x0 adaptation even when BN variables are present", {
  set.seed(2027)
  ref <- fairadapt(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                   prot.attr = "gender")
  
  set.seed(2027)
  mn <- fairadaptMinBN(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                       prot.attr = "gender", bn.set = c("test", "score"))
  
  expect_equal(mn$adapt0.train, ref$adapt.train)
})

test_that("non-BN variables use their x0 copy exactly", {
  set.seed(2028)
  mn <- fairadaptMinBN(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                       prot.attr = "gender", bn.set = c("test", "score"))
  
  expect_equal(mn$adapt.train$edu, mn$adapt0.train$edu)
})

test_that("the outcome can be a BN variable", {
  set.seed(2029)
  mn0 <- fairadaptMinBN(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                        prot.attr = "gender", bn.set = character(0L))
  
  set.seed(2029)
  mny <- fairadaptMinBN(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                        prot.attr = "gender", bn.set = "score")
  
  expect_false(isTRUE(all.equal(mny$adapt.train$score, mn0$adapt.train$score)))
})

test_that("min-BN and max-BN differ on the university graph", {
  set.seed(2030)
  mx <- fairadapt(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                  prot.attr = "gender", res.vars = "test")
  
  set.seed(2030)
  mn <- fairadaptMinBN(score ~ ., train.data = uni_dat, adj.mat = uni_adj,
                       prot.attr = "gender", bn.set = "test")
  
  expect_equal(mx$adapt.train$test, uni_dat$test)
  expect_false(isTRUE(all.equal(mn$adapt.train$test, uni_dat$test)))
  expect_false(isTRUE(all.equal(mx$adapt.train$score, mn$adapt.train$score)))
})

# -----------------------------------------------------------------------------
# II. Linear ground truth for the min-BN and max-BN path criteria
# -----------------------------------------------------------------------------

# Simulate the fully connected two-mediator linear SCM.
simTwoMediator <- function(n, cf, sd = 1) {
  x <- rbinom(n, 1L, 0.5)
  w1 <- cf$b1 * x + rnorm(n, sd = sd)
  w2 <- cf$b2 * x + cf$g * w1 + rnorm(n, sd = sd)
  y <- cf$aX * x + cf$a1 * w1 + cf$a2 * w2 + rnorm(n, sd = sd)
  
  data.frame(y = y, x = factor(x), w1 = w1, w2 = w2)
}

cf <- list(b1 = 1.0, b2 = 0.8, g = 0.6, aX = 1.2, a1 = 0.9, a2 = 0.7)

dim2 <- c("y", "x", "w1", "w2")
adj2 <- matrix(0L, 4L, 4L, dimnames = rep(list(dim2), 2L))
adj2["x", c("w1", "w2", "y")] <- 1L
adj2["w1", c("w2", "y")] <- 1L
adj2["w2", "y"] <- 1L

# Store path products and all descendant nodes on each path, including target.
coef_y <- with(cf, c(pi0 = aX, pi1 = b1 * a1, pi2 = b2 * a2,
                     pi3 = b1 * g * a2))
nodes_y <- list(
  pi0 = "y",
  pi1 = c("w1", "y"),
  pi2 = c("w2", "y"),
  pi3 = c("w1", "w2", "y")
)

coef_w2 <- with(cf, c(pi0 = b2, pi1 = b1 * g))
nodes_w2 <- list(
  pi0 = "w2",
  pi1 = c("w1", "w2")
)

# Run one linear case and compare estimated gaps with exact path products.
runCase <- function(B, type, n = 2e4, seed = 1L) {
  set.seed(seed)
  dat <- simTwoMediator(n, cf)
  
  fit <- fitBN(type, y ~ ., dat, adj2, B, quant.method = linearQuants)
  
  data.frame(
    criterion = type,
    bn.set = if (length(B)) paste(B, collapse = "+") else "none",
    target = c("y", "w2"),
    theory = c(
      retainedSum(B, type, coef_y, nodes_y),
      retainedSum(B, type, coef_w2, nodes_w2)
    ),
    estimate = c(adaptGap(fit, dat, "y"), adaptGap(fit, dat, "w2"))
  )
}

# Evaluate sets that distinguish target inclusion, min/max, and closure behavior.
lin_grid <- rbind(
  runCase(character(0L), "min"),
  runCase(character(0L), "max"),
  runCase("w2", "min"),
  runCase("w2", "max"),
  runCase("w1", "min"),
  runCase("w1", "max"),
  runCase("y", "min"),
  runCase("y", "max"),
  runCase(c("w1", "y"), "min"),
  runCase(c("w1", "y"), "max"),
  runCase(c("w2", "y"), "min"),
  runCase(c("w2", "y"), "max"),
  runCase(c("w1", "w2", "y"), "min"),
  runCase(c("w1", "w2", "y"), "max")
)

lin_grid$abs.err <- abs(lin_grid$theory - lin_grid$estimate)
print(lin_grid, digits = 3L, row.names = FALSE)

test_that("min-BN and max-BN match the exact linear path oracle", {
  expect_equal(lin_grid$estimate, lin_grid$theory, tolerance = 0.06)
})

test_that("the target node participates in both BN criteria", {
  d <- subset(lin_grid, bn.set == "w2" & target == "y")
  
  expect_equal(d$theory[d$criterion == "min"], 0)
  expect_equal(d$theory[d$criterion == "max"],
               unname(coef_y["pi2"] + coef_y["pi3"]))
})

test_that("B = {w2} isolates the mixed path at target w2", {
  d <- subset(lin_grid, bn.set == "w2" & target == "w2")
  gap <- d$estimate[d$criterion == "max"] - d$estimate[d$criterion == "min"]
  
  expect_equal(gap, with(cf, b1 * g), tolerance = 0.06)
})

test_that("a BN outcome alone retains exactly the direct effect under min-BN", {
  d <- subset(lin_grid, bn.set == "y" & criterion == "min" & target == "y")
  
  expect_equal(d$estimate, cf$aX, tolerance = 0.06)
})

test_that("all descendants BN makes min-BN and max-BN factual", {
  d <- subset(lin_grid, bn.set == "w1+w2+y" & target == "y")
  total <- sum(coef_y)
  
  expect_equal(d$estimate, rep(total, 2L), tolerance = 0.06)
})

# -----------------------------------------------------------------------------
# III. Two-copy routing and forward-closure logic
# -----------------------------------------------------------------------------

test_that("the closure examples have the intended graph structure", {
  expect_true(isFwdClosed(c("w2", "y"), adj2))
  expect_true(isFwdClosed("y", adj2))
  expect_false(isFwdClosed(c("w1", "y"), adj2))
})

test_that("a non-forward-closed BN set genuinely needs two values of a BN node", {
  set.seed(31)
  dat <- simTwoMediator(3000L, cf)
  fit <- fairadaptMinBN(y ~ ., train.data = dat, adj.mat = adj2,
                        prot.attr = "x", bn.set = c("w1", "y"),
                        quant.method = linearQuants)
  
  # w1 needs w1(0) for non-BN w2 and w1(m) for BN y.
  expect_false(isTRUE(all.equal(fit$adapt0.train$w1, fit$adapt.train$w1)))
  
  # w2 is non-BN and therefore equals its full x0 copy.
  expect_equal(fit$adapt.train$w2, fit$adapt0.train$w2)
  
  # y is BN and therefore differs from its full x0 copy.
  expect_false(isTRUE(all.equal(fit$adapt.train$y, fit$adapt0.train$y)))
})

test_that("the recanting-witness case retains only paths with all nodes BN", {
  d <- subset(lin_grid, bn.set == "w1+y" & criterion == "min" & target == "y")
  truth <- cf$aX + cf$b1 * cf$a1
  
  expect_equal(d$estimate, truth, tolerance = 0.06)
})

test_that("forward-closed B = {w2,y} kills every path entering w2 through w1", {
  d <- subset(lin_grid, bn.set == "w2+y" & criterion == "min" & target == "y")
  truth <- cf$aX + cf$b2 * cf$a2
  
  expect_equal(d$estimate, truth, tolerance = 0.06)
})

# -----------------------------------------------------------------------------
# IV. Spurious-effect reorientation and fairadaptAgent()
# -----------------------------------------------------------------------------

test_that("reorientSpurious() creates the complete forward X-Z graph", {
  nm <- c("x", "z1", "z2", "w")
  adj <- matrix(0L, 4L, 4L, dimnames = rep(list(nm), 2L))
  cfd <- adj
  
  adj["x", "w"] <- 1L
  adj["z1", "w"] <- 1L
  adj["z2", "w"] <- 1L
  cfd["x", "z1"] <- cfd["z1", "x"] <- 1L
  cfd["x", "z2"] <- cfd["z2", "x"] <- 1L
  
  out <- reorientSpurious(adj, cfd, "x", top.ord = nm)
  
  expect_equal(out$z.vars, c("z1", "z2"))
  expect_equal(out$adj.mat["x", c("z1", "z2")], c(z1 = 1L, z2 = 1L))
  expect_equal(out$adj.mat["z1", "z2"], 1L)
  expect_equal(out$cfd.mat[c("x", "z1", "z2"), c("x", "z1", "z2")],
               matrix(0L, 3L, 3L,
                      dimnames = list(c("x", "z1", "z2"),
                                      c("x", "z1", "z2"))))
})

test_that("max-BN uses a preliminary min-BN stage but min-BN does not", {
  cfd <- uni_adj
  cfd[,] <- 0L
  cfd["gender", "edu"] <- cfd["edu", "gender"] <- 1L
  
  adj <- uni_adj
  adj["gender", "edu"] <- 0L
  
  set.seed(40)
  mx <- fairadaptAgent(score ~ ., train.data = uni_dat, adj.mat = adj,
                       cfd.mat = cfd, prot.attr = "gender", bn.set = "test",
                       bn.type = "max", remove.SE = TRUE)
  
  set.seed(40)
  mn <- fairadaptAgent(score ~ ., train.data = uni_dat, adj.mat = adj,
                       cfd.mat = cfd, prot.attr = "gender", bn.set = "test",
                       bn.type = "min", remove.SE = TRUE)
  
  expect_equal(mx$z.vars, "edu")
  expect_equal(mn$z.vars, "edu")
  expect_false(is.null(mx$se.adapt.train))
  expect_null(mn$se.adapt.train)
  expect_true(all(c("edu", "test") %in% mx$res.vars))
  expect_equal(mn$bn.set, "test")
})

# Simulate a linear observed law generated by a latent common cause of x and z.
simSpurious <- function(n, s = 1.1, b = 0.9, g = 0.8, sd = 1) {
  u <- rbinom(n, 1L, 0.5)
  x <- u
  z <- s * u + rnorm(n, sd = sd)
  w <- b * x + g * z + rnorm(n, sd = sd)
  
  data.frame(w = w, x = factor(x), z = z)
}

sp_dim <- c("w", "x", "z")
sp_adj <- matrix(0L, 3L, 3L, dimnames = rep(list(sp_dim), 2L))
sp_cfd <- sp_adj
sp_adj["x", "w"] <- 1L
sp_adj["z", "w"] <- 1L
sp_cfd["x", "z"] <- sp_cfd["z", "x"] <- 1L

test_that("SE removal deletes X<->Z->W while preserving direct X->W when W is BN", {
  set.seed(50)
  dat <- simSpurious(2e4)
  
  mn <- fairadaptAgent(w ~ ., train.data = dat, adj.mat = sp_adj,
                       cfd.mat = sp_cfd, prot.attr = "x", bn.set = "w",
                       bn.type = "min", remove.SE = TRUE,
                       quant.method = linearQuants)
  
  mx <- fairadaptAgent(w ~ ., train.data = dat, adj.mat = sp_adj,
                       cfd.mat = sp_cfd, prot.attr = "x", bn.set = "w",
                       bn.type = "max", remove.SE = TRUE,
                       quant.method = linearQuants)
  
  expect_equal(adaptGap(mn, dat, "w"), 0.9, tolerance = 0.06)
  expect_equal(adaptGap(mx, dat, "w"), 0.9, tolerance = 0.06)
})

test_that("with no BN variables the agent removes both spurious and causal variation", {
  set.seed(51)
  dat <- simSpurious(2e4)
  
  mn <- fairadaptAgent(w ~ ., train.data = dat, adj.mat = sp_adj,
                       cfd.mat = sp_cfd, prot.attr = "x", bn.set = NULL,
                       bn.type = "min", remove.SE = TRUE,
                       quant.method = linearQuants)
  
  mx <- fairadaptAgent(w ~ ., train.data = dat, adj.mat = sp_adj,
                       cfd.mat = sp_cfd, prot.attr = "x", bn.set = NULL,
                       bn.type = "max", remove.SE = TRUE,
                       quant.method = linearQuants)
  
  expect_equal(adaptGap(mn, dat, "w"), 0, tolerance = 0.06)
  expect_equal(adaptGap(mx, dat, "w"), 0, tolerance = 0.06)
})
