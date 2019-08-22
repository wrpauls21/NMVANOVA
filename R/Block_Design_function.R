BlockD<-function(r,k,f,n){
  tm = gl(k, 1, n*k, factor(f));   # matching treatment
  tm;

  blk = gl(n, k, k*n);             # blocking factor
  blk;

  av = aov(r ~ tm+blk);
  return(summary(av));

  av1 = aov(r ~ tm);
  return(summary(av1));

  
}

