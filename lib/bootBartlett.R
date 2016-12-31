#' Bootstrapped Barletts test
bootBartlett <- function(x1, x2, n) {
  (Tobs <- bartlett.test(list(x1, x2))$statistic)
  ratios <- rep(NA, n)
  for (i in 1:n) {
    ratios[i] <- bartlett.test(list(sample(x1, length(x1), replace=T), sample(x2, length(x2),replace=T)))$statistic 
  } 
  hist(ratios)
  quantile(ratios, c(0.025, 0.975))
}