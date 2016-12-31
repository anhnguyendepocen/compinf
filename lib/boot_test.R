#' one-sided two sample bootstrapped test for diff in mean
boot_test<-function(x,y,n){
  alldata<-c(x,y)
  nx<-length(x)
  ny<-length(y)
  t.gen<-rep(0,n)
  for(i in 1:n){
    newx<-sample(alldata,nx,replace=T)
    newy<-sample(alldata,ny,replace=T)
    t.gen[i]<-t.test(newx,newy,alternative="greater")$statistic
  }
  hist(t.gen,col=0,prob=T,ylim=c(0,0.4))
  z<-seq(from=min(t.gen),to=max(t.gen),length=100)
  lines(z,dt(z,14))
  mean(t.gen>=t.test(x,y,alternative="greater")$statistic)
}