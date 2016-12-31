#compare sample variance of ceramic strength from two different batchs.
batch1 <- Ceramic[Ceramic$batch==1,]$strength
batch2 <- Ceramic[Ceramic$batch==2,]$strength

#the distrbutions of each batch are not Normal
mypar(mfrow=c(2,1))
hist(batch1, xlim=c(300,900), xlab="Batch 1 Strength", main="",nclass=19)
abline(v=mean(batch1), col='blue')
abline(v=c(mean(batch1)-sd(batch1), mean(batch1)+sd(batch1)), col='red')
hist(batch2, xlim=c(300,900), xlab="Batch 2 Strength", main="",nclass=19)
abline(v=mean(batch2), col='blue')
abline(v=c(mean(batch2)-sd(batch2), mean(batch2)+sd(batch2)), col='red')
mypar(mfrow=c(1,1))

#again using quantile plots
qqnorm(batch1)
qqnorm(batch2)

#variance of batch 1 appears greater than batch 2
var(batch1)/var(batch2)

#assess how far the ratio is from 1
#standard tests suggest no difference but distributions assumptions are not met
var.test(batch1, batch2)
bartlett.test(list(batch1, batch2))
bt <- bartlett.test(strength~batch, Ceramic)
bt$statistic
fligner.test(list(batch1, batch2))


#95% CIs contains 1, therefore there is no evidence to reject null hypothesis
t1 <- twoSampleVarTest(batch1, batch2, 10000, varRatio)
t1

#same inference from bootstapped Barlett
bootBartlett(batch1, batch2, 10000)









