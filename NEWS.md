# fairadapt 0.2.1
Non-breaking changes:
- `fairTwins()` now has a default `train.id` of `seq_len(nrow(x$train))` (previous default was `1L`)
- `print.fairadapt()` has been changed to print the "Formula", not the "Call" (previously incorrect)

# fairadapt 0.2.0
Breaking changes:
- `protect.A` argument renamed to `prot.attr`
- argument ordering changed; `prot.attr` moved to position number two, `adj.mat`
argument moved to position three

Non-breaking changes:
- `top.ord` argument introduced, for running adaptation with topological ordering,
without the need for `adj.mat`
- `quant.method` argument now a function, allowing for custom user quantile regression
methods (via S3 dispatch)
- `visualize.graph` argument available for plotting the causal diagram
