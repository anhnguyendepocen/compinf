library(ProjectTemplate)
load.project()

#initial estimates of mean from visual inspection of histogram
mu <- c(-3, 2, 5) # the mean
phi <- c(1/3,1/3,1/3) # the weight of each distribution
emResults <- em(mixture_normal,mu,phi)

#plot the histogram with density function and parameter estimates
hist(mixture_normal, freq=FALSE, main="", xlab="mixturenormal values")
#histograms suggest that there are 3 normal distributions centered approx. on -4, 2, 6

lines(smooth.spline(mixture_normal, emResults$densityAl, spar=0.35), col="blue", lwd=2)
abline(v=emResults$mu, lty="dotted", col="red")

#run multiple times using random initial values to test sensitivity and look at convergence times
#TODO: parallelise for speed
n <- 1000
results <- matrix(nrow=n, ncol=13,byrow=T)
colnames(results) <- c("initial mu1", "initial mu2", "initial mu3", "initial phi1", "initial phi2", "initial phi3","mu1", "mu2", "mu3", "phi1", "phi2", "phi3", "iterations")
for (m in 1:n) {
  res <- em(mixture_normal,
            mu=runif(3, min(mixture_normal), max(mixture_normal)), 
            phi=runif(3))
  results[m,1:3] <- (res$initialMu)
  results[m,4:6] <- (res$initialPhi)
  results[m,7:9] <- (res$mu)
  results[m,10:12] <- (res$phi)
  results[m,13] <- res$iterations
  message("m = ", m)
}

results <- data.frame(results)
hist(results$iterations)
