
with_seed <- function(seed, code, rng_kind = "default",
                      rng_normal_kind = rng_kind, rng_sample_kind = rng_kind) {
  
  code <- substitute(code)
  
  old_seed <- get_seed()
  new_seed <- list(
    seed = seed,
    rng_kind = c(rng_kind, rng_normal_kind, rng_sample_kind)
  )
  
  if (is.null(old_seed)) {
    on.exit(rm_seed())
  } else {
    on.exit(set_seed(old_seed))
  }
  
  set_seed(new_seed)
  
  eval.parent(code)
}

# the following utilities originate from withr

has_seed <- function() {
  exists(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

get_seed <- function() {
  
  if (!has_seed()) {
    return(NULL)
  }
  
  seed <- get(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  
  list(random_seed = seed, rng_kind = RNGkind())
}

set_seed <- function(seed) {
  
  if (getRversion() < "3.6") {
    seed$rng_kind <- seed$rng_kind[1L:2L]
  }
  if (is.null(seed$seed)) {
    do.call(RNGkind, args = as.list(seed$rng_kind))
    assign(".Random.seed", seed$random_seed, globalenv())
  } else {
    do.call(RNGkind, args = as.list(seed$rng_kind))
    set.seed(seed$seed)
  }
}

rm_seed <- function() {
  
  if (!has_seed()) {
    return(NULL)
  }
  
  set.seed(seed = NULL)
  
  rm(".Random.seed", envir = globalenv())
}