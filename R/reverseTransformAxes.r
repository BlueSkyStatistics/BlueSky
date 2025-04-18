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
