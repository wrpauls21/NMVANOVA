RManovaNov<-function(r,k,f,n){
  tm = gl(k, 1, n*k, unique(factor(f)))   # matching treatment
  tm

  blk = gl(n, k, k*n)           # blocking factor
  blk

  av = aov(r ~ blk+tm)
  summary(av)

  qqnorm(av$residuals)
  plot(av$fitted.values,av$residuals)

  TukeyHSD(av)$tm
}

