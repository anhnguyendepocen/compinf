#' Estimates difference in sample variance from two samples.
twoSampleVarTest <- function(x1, x2, n, test) {
  Tobs <- test(x1, x2)
  ratios <- rep(NA, n)
  for (i in 1:n) {
    x1n <- sample(x1, length(x1), replace=T)
    x2n <- sample(x2, length(x2), replace=T)
    ratios[i] <- test(x1n, x2n)
  } 
  hist(ratios, xlab="var(batch 1)/var(batch 2)", main="",nclass=19)
  abline(v=Tobs, col='green')
  abline(v=mean(ratios), col='blue')
  cis <- quantile(ratios, c(0.025, 0.975))  
  abline(v=cis, col='red')
  list("CI"=cis, "Tobs"=Tobs)
}

