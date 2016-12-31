#test differences in sample variance using simulated data
var.test(rnorm(240, 0, 1), rnorm(240, 0, 1.1))
t2 <- twoSampleVarTest(rnorm(240, 0, 1), rnorm(240, 0, 1.1), 10000, varRatio)
t2$CI
t2$p
t2$Tobs

t2 <- twoSampleVarTest(rexp(240, 1), rexp(240, 2), 100000, varRatio)
t2$CI
t2$p

bartlett.test(list(rnorm(240, 0, 1), rnorm(240, 0, 1.1)))