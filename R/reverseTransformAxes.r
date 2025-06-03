log_reverse_trans <- scales::trans_new(
  name = "log-reverse",
  transform = function(x) -log(x),
  inverse = function(x) exp(-x),
  domain = c(1e-100, Inf)
)

log10_reverse_trans <- scales::trans_new(
  name = "log10-reverse",
  transform = function(x) -log10(x),
  inverse = function(x) 10^(-x),
  domain = c(1e-100, Inf)
)



log2_reverse_trans <- scales::trans_new(
  name = "log2-reverse",
  transform = function(x) -log2(x),
  inverse = function(x) 2^(-x),
  domain = c(2^-100, Inf)  # or c(.Machine$double.eps, Inf) for numerical stability
)

log2_trans <- scales::trans_new(
  name = "log2",
  transform = function(x) log2(x),
  inverse = function(x) 2^x,
  domain = c(.Machine$double.eps, Inf)  # avoid log2(0) or negatives
)