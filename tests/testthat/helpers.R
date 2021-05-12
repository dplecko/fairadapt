
dataGen <- function(n, add_z = FALSE, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  a <- rbinom(n, size = 1, prob = 0.5)

  coeff <- 1 / 4
  dev   <- 1

  x <-  -a * coeff + coeff / 2 + rnorm(n, sd = dev)
  y <- rbinom(n, size = 1, prob = expit(x))

  res <- data.frame(y = factor(y), a = a, x = x)

  if (add_z) {
    res <- cbind(res, z = rnorm(n))
  }

  res
}

totVar <- function(x, what, var) {

  ind <- x[["base.ind"]][seq_len(nrow(x[[what]]))]
  dat <- x[[what]][[var]]

  if (is.factor(dat)) {
    dat <- as.numeric(as.character(dat))
  }

  signif(mean(dat[ind]) - mean(dat[!ind]))
}

sem <- function(f, a, e) {

  x <- vector("list", length(f))
  names(x) <- names(f)

  for (i in seq_along(f)) {
    x[[i]] <- f[[i]](a, x, e[, i])
  }

  as.data.frame(c(list(a = a), x))
}

save_png <- function(code, width = 400, height = 400) {

  path <- tempfile(fileext = ".png")

  png(path, width = width, height = height)
  on.exit(dev.off())

  code

  path
}

save_csv <- function(data) {

  path <- tempfile(fileext = ".png")

  write.csv(data, path)

  path
}

expit <- function(x) exp(x)/(1+exp(x))

skip_on_r_version <- function(min_version = "3.6.0") {
  if (getRversion() < min_version) {
    skip(paste("R version <", min_version))
  }
}

expect_snapshot_plot <- function(name, code) {

  skip_on_os(c("windows", "linux", "solaris"))

  path <- save_png(code)
  expect_snapshot_file(path, paste0(name, ".png"))
}

expect_snapshot_csv <- function(name, code) {

  skip_on_r_version("3.6.0")

  path <- save_csv(code)
  expect_snapshot_file(path, paste0(name, ".csv"))
}

expect_snapshot_json <- function(code) {

  skip_on_r_version("3.6.0")

  expect_snapshot_value(code, style = "json2")
}
