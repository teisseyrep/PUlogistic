logLike_w = function(par,x,y,c){
  beta0 = par[1]
  beta1 = par[-1]
  res = -sum(  (y/c)*log(sigma(x%*%beta1+beta0))+(1-y/c)*log(1-sigma(x%*%beta1+beta0))   )
  return(res)
}
gr_w = function(par,x,y,c){
  beta0 = par[1]
  beta1 = par[-1]
  
  var1 = sigma(x%*%beta1+beta0)*(1-sigma(x%*%beta1+beta0))
  sigma1 = sigma(x%*%beta1+beta0)
  
  a= var1*( (-y/c+sigma1)/(sigma1*(1-sigma1)) )
  
  res = t(cbind(1,x))%*%a
  return(res)
}

logistic_fit_w = function(x,y,c){
  
  if(c==0) c=c+0.001
  
  optim1 = optim(par=rep(0,ncol(x)+1),fn=logLike_w,gr=gr_w,x=x,y=y,c=c,method="BFGS")
  par = optim1$par
  return(list(par=par))
}
