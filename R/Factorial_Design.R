FactD<-function(r,k1,k2,f1,f2,n){
  tm1 = gl(k1, 1, n*k1*k2, factor(f1)); 
  tm1; 
  
  #Similarly, create a vector that corresponds to the 2nd treatment level 
  #of the response data r;
  
  tm2 = gl(k2, n*k1, n*k1*k2, factor(f2)); 
  tm2; 
  
  #Apply the function aov to a formula that describes the response r 
  #by the two treatment factors tm1 and tm2 with interaction;
  
  av = aov(r ~ tm1 * tm2);  # include interaction
  summary(av); 
}

