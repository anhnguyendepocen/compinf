#' 1D expectation maximisation
#' 
#' @param x The mixture data
#' @param mu The initial estimate of the means
#' @param phi The initial estimates of the weights
#' @return A list with the results
em <- function(x, mu, phi, maxIterations=1000) {
  #save the initial estimates for monitoring the results
  initialMu <- mu
  initialPhi <- phi
  #K, the number of component distributions
  numberOfDistributions <- length(mu)
  
  densityK <- matrix(nrow=length(x), ncol=length(mu),byrow=T)
  densityAll <- rep(0, length(x))
  converged <- FALSE
  #variables used for monitoring convergence
  i <- 0
  prevMu <- rep(0, numberOfDistributions)
  prevPhi <- rep(0, numberOfDistributions)
  
  #continue until all parameter estimates have converged
  while(!converged)
  {
    i <- i+1
    #E-Step: calulate the likelihood that each point belongs to each distribution 
    densityAll <- rep(0, length(x))
    for(k in 1:numberOfDistributions)
    {
      densityAll <- densityAll + (dnorm(x, mu[k], 1)*phi[k])
    }
    
    for(k in 1:numberOfDistributions)
    {
      densityK[,k] <- (dnorm(x, mu[k], 1)*phi[k])/densityAll
    }
    
    #M-Step: use the likelihood to update the parameter estimates
    for(k in 1:numberOfDistributions)
    {
      mu[k] <- sum(densityK[,k]*x)/sum(densityK[,k])  
      phi[k] <- sum(densityK[,k])/length(x)
    }
    
    #check if all parameters have converged or the max iterations has been reached
    converged <- (mu - prevMu <10^-10) && (phi - prevPhi == 0)
    if (i > maxIterations)
    {
      converged = T
      message("Exiting as parameters are converging to slowly: diff mu = ", mu - prevMu,  " diff phi = ", phi - prevPhi)
    }
    prevMu <- mu
    prevPhi <- phi
  }
  list("densityAll"=densityAll, "densityK"=densityK, "mu"=mu, "phi"=phi, "iterations"=i, "initialMu"= initialMu,
       "initialPhi"=initialPhi)
}