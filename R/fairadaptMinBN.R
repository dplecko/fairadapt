# Evaluate code without letting auxiliary counterfactual work advance the RNG state.
#' @export
preserveRNG <- function(expr) {
  
  has.seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  
  if (has.seed) {
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  
  on.exit({
    if (has.seed) {
      assign(".Random.seed", seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  force(expr)
}

# Switch a split quantile engine from the x0 target world to the x1 target world.
#' @export
setWorld <- function(object, lab1) {
  
  if (!lab1) {
    return(object)
  }
  
  if (inherits(object, c("rangersplit", "quantregsplit"))) {
    object$class0 <- object$class1
  }
  
  object
}

# Evaluate the factual latent quantile in either the x0 or x1 target world.
computeWorldQuants <- function(object, qr.data, cf.parents, ind, lab1,
                               prot.attr, curr.parents) {
  
  object <- setWorld(object, lab1)
  
  if (lab1 && is.element(prot.attr, curr.parents)) {
    cf.parents[, prot.attr] <- qr.data[!ind, prot.attr]
  }
  
  if (lab1 && !inherits(object, c("rangersplit", "quantregsplit"))) {
    assert_that(
      is.element(prot.attr, curr.parents),
      msg = paste("a non-split quantile engine needs the protected attribute",
                  "among the covariates; use `rangerQuants` or `linearQuants`.")
    )
  }
  
  computeQuants(object, qr.data, cf.parents, ind)
}

# Replace the bidirected X-component by a fully connected forward DAG rooted at X.
#' @export
reorientSpurious <- function(adj.mat, cfd.mat, prot.attr, top.ord = NULL) {
  
  if (is.null(cfd.mat)) {
    cfd.mat <- adj.mat
    cfd.mat[, ] <- 0
  }
  
  if (is.null(top.ord)) {
    top.ord <- topologicalOrdering(adj.mat)
  }
  
  z.vars <- setdiff(confoundedComponent(prot.attr, cfd.mat), prot.attr)
  
  if (length(z.vars) == 0L) {
    return(list(adj.mat = adj.mat, cfd.mat = cfd.mat,
                top.ord = top.ord, z.vars = z.vars))
  }
  
  z.anc <- vapply(z.vars, function(z) {
    is.element(prot.attr, getDescendants(z, adj.mat))
  }, logical(1L))
  
  assert_that(
    !any(z.anc),
    msg = "cannot reorient a confounded variable that is an ancestor of the protected attribute"
  )
  
  z.vars <- top.ord[is.element(top.ord, z.vars)]
  cfd.comp <- c(prot.attr, z.vars)
  
  adj.mat[cfd.comp, cfd.comp] <- 0L
  cfd.mat[cfd.comp, cfd.comp] <- 0L
  
  for (j in seq_along(cfd.comp)) {
    if (j < length(cfd.comp)) {
      adj.mat[cfd.comp[j], cfd.comp[(j + 1L):length(cfd.comp)]] <- 1L
    }
  }
  
  top.ord <- topologicalOrdering(adj.mat)
  
  list(adj.mat = adj.mat, cfd.mat = cfd.mat,
       top.ord = top.ord, z.vars = z.vars)
}

# Perform fair adaptation under the min-BN criterion.
#' @export
fairadaptMinBN <- function(formula, prot.attr, adj.mat, train.data,
                           test.data = NULL, cfd.mat = NULL, top.ord = NULL, bn.set = NULL,
                           quant.method = rangerQuants, visualize.graph = FALSE, eval.qfit = NULL, ...) {
  
  if (missing(adj.mat)) {
    adj.mat <- NULL
  }
  
  # Verify correctness of input.
  correctInput(formula, train.data, test.data, adj.mat, cfd.mat, top.ord,
               prot.attr, bn.set, quant.method)
  
  # Reorder the adjacency matrix if necessary.
  adj.mat <- adj.mat[colnames(adj.mat), ]
  
  # Reorder columns and factorize the protected attribute.
  train.data <- model.frame(formula, train.data)
  train.data[, prot.attr] <- as.factor(train.data[, prot.attr])
  train.len <- nrow(train.data)
  
  if (!is.null(test.data)) {
    test.data <- test.data[, colnames(train.data)[-1L]]
    test.data[, prot.attr] <- as.factor(test.data[, prot.attr])
    test.data <- cbind(NA, test.data)
    colnames(test.data) <- colnames(train.data)
  }
  
  # Combine training and testing data for a common adaptation pass.
  org.data <- rbind(train.data, test.data)
  full.len <- nrow(org.data)
  
  # Keep only graph variables that occur in the model frame.
  adj.mat <- adj.mat[colnames(org.data), colnames(org.data)]
  
  if (is.null(cfd.mat) && !is.null(adj.mat)) {
    cfd.mat <- adj.mat
    cfd.mat[, ] <- 0
  } else if (!is.null(cfd.mat)) {
    cfd.mat <- cfd.mat[colnames(org.data), colnames(org.data)]
  }
  
  # Treat a missing BN set as an empty set.
  if (is.null(bn.set)) {
    bn.set <- character(0L)
  }
  
  # Construct the full x0 and min-BN counterfactual copies.
  adapt0.data <- org.data
  adapt.data <- org.data
  base.lvl <- levels(org.data[, prot.attr])[1L]
  attr.lvls <- levels(org.data[, prot.attr])
  base.ind <- org.data[, prot.attr] == base.lvl
  adapt0.data[, prot.attr] <- factor(base.lvl,
                                     levels = levels(org.data[, prot.attr]))
  adapt.data[, prot.attr] <- factor(base.lvl,
                                    levels = levels(org.data[, prot.attr]))
  
  # Obtain the topological ordering and descendants of the protected attribute.
  if (is.null(top.ord)) {
    top.ord <- topologicalOrdering(adj.mat)
    attr.des <- getDescendants(prot.attr, adj.mat)
    attr.root <- (length(getParents(prot.attr, adj.mat)) == 0L) &&
      (length(confoundedComponent(prot.attr, cfd.mat)) == 1L)
    ig <- graphModel(adj.mat, cfd.mat)
    
    if (visualize.graph) {
      plot(ig)
    }
  } else {
    attr.des <- getDescendants(prot.attr, adj.mat, top.ord)
    attr.root <- top.ord[1L] == prot.attr
    ig <- NULL
  }
  
  # Require every BN variable to be a descendant of the protected attribute.
  assert_that(
    all(is.element(bn.set, attr.des)),
    msg = "`bn.set` must be a subset of the descendants of the protected attribute"
  )
  
  q.engine <- list()
  
  # Traverse all variables after the protected attribute in topological order.
  var.ind <- seq.int(which(top.ord == prot.attr) + 1L, length(top.ord))
  
  for (curr.var in top.ord[var.ind]) {
    
    # Skip variables that are not descendants of the protected attribute.
    if (!is.element(curr.var, attr.des)) {
      next
    }
    
    q.engine[[curr.var]] <- list()
    
    # Record the original variable type for later decoding.
    type <- class(org.data[, curr.var])
    q.engine[[curr.var]][["type"]] <- type
    
    discrete <- FALSE
    q.engine[[curr.var]][["discrete"]] <- discrete
    
    # Obtain the adjustment set used by the original fairadapt implementation.
    curr.parents <- adjustmentSet(curr.var, adj.mat, cfd.mat, top.ord)
    q.engine[[curr.var]][["parents"]] <- curr.parents
    q.engine[[curr.var]][["bn"]] <- is.element(curr.var, bn.set)
    
    row.idx <- rep(TRUE, full.len)
    
    # Exclude test rows when adapting the response variable.
    if (curr.var == colnames(train.data)[1L]) {
      row.idx[-seq_len(train.len)] <- FALSE
    }
    
    # Encode discrete variables on a continuous latent scale.
    if (length(unique(org.data[, curr.var])) < 10 |
        is.factor(org.data[, curr.var]) | is.integer(org.data[, curr.var]) |
        is.character(org.data[, curr.var])) {
      
      discrete <- TRUE
      q.engine[[curr.var]][["discrete"]] <- discrete
      
      if (is.character(org.data[, curr.var])) {
        org.data[, curr.var] <- factor(org.data[, curr.var])
      }
      
      if (is.factor(org.data[, curr.var])) {
        org.data[, curr.var] <- factor(
          org.data[, curr.var], levels = catOrder(org.data[row.idx, 1L],
                                                  org.data[row.idx, curr.var])
        )
      } else if (is.integer(org.data[, curr.var])) {
        q.engine[[curr.var]][["discrete"]] <- discrete <- 1L
      } else {
        org.data[, curr.var] <- factor(org.data[, curr.var])
      }
      
      unique.values <- levels(org.data[, curr.var])
      q.engine[[curr.var]][["unique.values"]] <- unique.values
      
      int.enc <- as.integer(org.data[row.idx, curr.var]) +
        runif(length(org.data[row.idx, curr.var]), -0.5, 0.5)
      
      adapt0.data[, curr.var] <- makeLength(int.enc, train.len, full.len)
      adapt.data[, curr.var] <- makeLength(int.enc, train.len, full.len)
      org.data[, curr.var] <- adapt0.data[, curr.var]
    }
    
    # Fit one quantile engine using the factual data.
    qr.data <- org.data[row.idx, c(curr.var, curr.parents), drop = FALSE]
    object <- quant.method(qr.data, attr.root, base.ind[row.idx], ...)
    q.engine[[curr.var]][["object"]] <- object
    
    # Compute the full x0 counterfactual copy.
    cf.parents0 <- adapt0.data[!base.ind & row.idx, curr.parents, drop = FALSE]
    
    assert_that(ncol(qr.data) == (ncol(cf.parents0) + 1L))
    
    adapt0.data[!base.ind & row.idx, curr.var] <-
      computeWorldQuants(object, qr.data, cf.parents0, base.ind[row.idx],
                         FALSE, prot.attr, curr.parents)
    
    # Enforce marginal matching for the full x0 copy of a discrete variable.
    if (discrete & attr.root) {
      adapt0.data[row.idx, curr.var] <-
        marginalMatching(adapt0.data[row.idx, curr.var], base.ind[row.idx])
    }
    
    if (is.element(curr.var, bn.set)) {
      
      # Start BN-parent evaluation from the full x0 parent copy.
      cf.parents <- adapt0.data[!base.ind & row.idx, curr.parents, drop = FALSE]
      
      # Replace BN parents by their forward min-BN counterfactual values.
      bn.parents <- intersect(curr.parents, bn.set)
      if (length(bn.parents) > 0L) {
        cf.parents[, bn.parents] <-
          adapt.data[!base.ind & row.idx, bn.parents, drop = FALSE]
      }
      
      # Evaluate a BN variable in the x1 world with mixed counterfactual parents.
      adapt.data[!base.ind & row.idx, curr.var] <-
        preserveRNG(
          computeWorldQuants(object, qr.data, cf.parents, base.ind[row.idx],
                             TRUE, prot.attr, curr.parents)
        )
      
    } else {
      
      # A non-BN variable equals its full x0 counterfactual copy.
      adapt.data[row.idx, curr.var] <- adapt0.data[row.idx, curr.var]
    }
    
    if (!is.null(eval.qfit)) {
      eval.qfit <- as.integer(eval.qfit)
      assert_that(eval.qfit > 1L,
                  msg = "`eval.qfit` argument must be a positive integer.")
      
      # Split the data into folds for quantile-fit evaluation.
      folds <- as.integer(cut(seq_row(qr.data), breaks = eval.qfit))
      q.fold <- c()
      
      for (fold in seq_len(eval.qfit)) {
        fold.ind <- folds == fold
        obj <- quant.method(qr.data[!fold.ind, ], A.root = FALSE, ind = NULL,
                            ...)
        
        q.fold <- rbind(
          q.fold, computeQuants(obj, qr.data[fold.ind, ], newdata = NULL,
                                ind = NULL, test = TRUE, emp.only = TRUE)
        )
      }
      
      q.engine[[curr.var]][["qfit.score"]] <-
        qfitScore(qr.data[[curr.var]], q.fold)
    }
    
    # Decode discrete variables back to their original representation.
    if (discrete) {
      
      if (is.integer(discrete)) {
        adapt0.data[, curr.var] <- as.integer(round(adapt0.data[, curr.var]))
        adapt.data[, curr.var] <- as.integer(round(adapt.data[, curr.var]))
        org.data[, curr.var] <- as.integer(round(org.data[, curr.var]))
      } else {
        adapt0.data[, curr.var] <-
          decodeDiscrete(adapt0.data[row.idx, curr.var], unique.values, type,
                         full.len)
        adapt.data[, curr.var] <-
          preserveRNG(
            decodeDiscrete(adapt.data[row.idx, curr.var], unique.values, type,
                           full.len)
          )
        org.data[, curr.var] <-
          decodeDiscrete(org.data[row.idx, curr.var], unique.values, type,
                         full.len)
      }
    }
  }
  
  structure(
    list(
      adapt.train = adapt.data[seq_len(train.len), ],
      adapt.test = adapt.data[-seq_len(train.len), -1L],
      adapt0.train = adapt0.data[seq_len(train.len), ],
      adapt0.test = adapt0.data[-seq_len(train.len), -1L],
      train = train.data,
      test = test.data,
      base.lvl = base.lvl,
      attr.lvls = attr.lvls,
      base.ind = base.ind,
      formula = formula,
      bn.set = bn.set,
      res.vars = NULL,
      prot.attr = prot.attr,
      graph = ig,
      quant.method = deparse(substitute(quant.method)),
      adapt.call = match.call(),
      adj.mat = adj.mat,
      cfd.mat = cfd.mat,
      top.ord = top.ord,
      q.engine = q.engine
    ),
    class = c("fairadaptMinBN", "fairadapt")
  )
}

# Dispatch between max-BN and min-BN adaptation with optional spurious-effect removal.
#' @export
fairadaptAgent <- function(formula, prot.attr, adj.mat, train.data,
                           test.data = NULL, cfd.mat = NULL, top.ord = NULL,
                           bn.type = c("max", "min"), remove.SE = FALSE, bn.set = NULL,
                           quant.method = rangerQuants, visualize.graph = FALSE, eval.qfit = NULL, ...) {
  
  bn.type <- match.arg(bn.type)
  
  # Treat a missing BN set as an empty set.
  if (is.null(bn.set)) {
    bn.set <- character(0L)
  }
  
  # Start from the original graph.
  adj.work <- adj.mat
  cfd.work <- cfd.mat
  top.work <- top.ord
  z.vars <- character(0L)
  
  # Reorient the protected confounded component when removing spurious effects.
  if (remove.SE) {
    reor <- reorientSpurious(adj.mat, cfd.mat, prot.attr, top.ord)
    adj.work <- reor$adj.mat
    cfd.work <- reor$cfd.mat
    top.work <- reor$top.ord
    z.vars <- reor$z.vars
  }
  
  # Without spurious variables, run the requested BN adaptation directly.
  if (length(z.vars) == 0L) {
    
    if (bn.type == "min") {
      fit <- fairadaptMinBN(
        formula = formula, prot.attr = prot.attr, adj.mat = adj.work,
        train.data = train.data, test.data = test.data, cfd.mat = cfd.work,
        top.ord = top.work, bn.set = bn.set, quant.method = quant.method,
        visualize.graph = visualize.graph, eval.qfit = eval.qfit, ...
      )
    } else {
      fit <- fairadapt(
        formula = formula, prot.attr = prot.attr, adj.mat = adj.work,
        train.data = train.data, test.data = test.data, cfd.mat = cfd.work,
        top.ord = top.work, res.vars = bn.set, quant.method = quant.method,
        visualize.graph = visualize.graph, eval.qfit = eval.qfit, ...
      )
    }
    
    fit$bn.type <- bn.type
    fit$bn.set <- bn.set
    fit$remove.SE <- remove.SE
    fit$z.vars <- z.vars
    fit$agent.call <- match.call()
    class(fit) <- c("fairadaptAgent", class(fit))
    
    return(fit)
  }
  
  # Min-BN removes spurious and non-BN causal paths in one pass.
  if (bn.type == "min") {
    
    fit <- fairadaptMinBN(
      formula = formula, prot.attr = prot.attr, adj.mat = adj.work,
      train.data = train.data, test.data = test.data, cfd.mat = cfd.work,
      top.ord = top.work, bn.set = setdiff(bn.set, z.vars),
      quant.method = quant.method, visualize.graph = visualize.graph,
      eval.qfit = eval.qfit, ...
    )
    
    fit$bn.type <- bn.type
    fit$bn.set <- bn.set
    fit$remove.SE <- TRUE
    fit$z.vars <- z.vars
    fit$agent.call <- match.call()
    class(fit) <- c("fairadaptAgent", class(fit))
    
    return(fit)
  }
  
  # Keep every reoriented descendant except Z in stage-one min-BN.
  bn.se <- setdiff(getDescendants(prot.attr, adj.work), z.vars)
  
  # Remove all variation transmitted through the reoriented Z variables.
  se.fit <- fairadaptMinBN(
    formula = formula, prot.attr = prot.attr, adj.mat = adj.work,
    train.data = train.data, test.data = test.data, cfd.mat = cfd.work,
    top.ord = top.work, bn.set = bn.se, quant.method = quant.method,
    visualize.graph = visualize.graph, eval.qfit = eval.qfit, ...
  )
  
  # Restore the factual protected attribute before stage two.
  stage.train <- se.fit$adapt.train
  stage.train[, prot.attr] <- se.fit$train[, prot.attr]
  
  # Restore the factual protected attribute in the test data as well.
  stage.test <- NULL
  if (!is.null(test.data)) {
    stage.test <- se.fit$adapt.test
    stage.test[, prot.attr] <- se.fit$test[, prot.attr]
  }
  
  # Run max-BN with the cleaned Z variables treated as resolvers.
  fit <- fairadapt(
    formula = formula, prot.attr = prot.attr, adj.mat = adj.work,
    train.data = stage.train, test.data = stage.test, cfd.mat = cfd.work,
    top.ord = top.work, res.vars = union(z.vars, bn.set),
    quant.method = quant.method, visualize.graph = visualize.graph,
    eval.qfit = eval.qfit, ...
  )
  
  # Store the agent-level metadata and the intermediate SE adaptation.
  fit$bn.type <- bn.type
  fit$bn.set <- bn.set
  fit$remove.SE <- TRUE
  fit$z.vars <- z.vars
  fit$se.adapt.train <- se.fit$adapt.train
  fit$se.adapt.test <- se.fit$adapt.test
  fit$original.train <- se.fit$train
  fit$original.test <- se.fit$test
  fit$agent.call <- match.call()
  class(fit) <- c("fairadaptAgent", class(fit))
  
  fit
}
