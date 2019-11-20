logLike = function(par,x,y){
  beta0 = par[1]
  beta1 = par[-1]
  res = -sum(  y*log(sigma(x%*%beta1+beta0))+(1-y)*log(1-sigma(x%*%beta1+beta0))   )
  return(res)
}
gr = function(par,x,y){
  beta0 = par[1]
  beta1 = par[-1]
  
  var1 = sigma(x%*%beta1+beta0)*(1-sigma(x%*%beta1+beta0))
  sigma1 = sigma(x%*%beta1+beta0)
  
  a= var1*( (-y+sigma1)/(sigma1*(1-sigma1)) )
  
  res = t(cbind(1,x))%*%a
  return(res)
}

logistic_fit = function(x,y){
  
  optim1 = optim(par=rep(0,ncol(x)+1),fn=logLike,gr=gr,x=x,y=y,method="BFGS")
  par = optim1$par
  return(list(par=par))
}
