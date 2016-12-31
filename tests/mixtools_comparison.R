#verify by comparing to mixtools
res <-normalmixEM(mixture_normal, lambda=c(1/3,1/3,1/3), mu =c(-3, 2, 5), sigma=c(1.5, 1.5, 1.5), k=3)
res$mu
res$sigma
res$lambda
