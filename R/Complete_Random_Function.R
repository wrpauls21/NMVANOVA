CRandom<-function(r,k,f,n){
  tm = gl(k, 1, n*k, factor(f));   # matching treatment
  tm;

  av = aov(r ~ tm);
  summary(av);

  TukeyHSD(av);
}
