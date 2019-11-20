


logLike_joint = function(par,x,y){
  beta0 = par[1]
  beta1 = par[-c(1,length(par))]
  c1 = par[length(par)]
  term1 = c1*sigma(x%*%beta1+beta0)
  term2 = 1-c1*sigma(x%*%beta1+beta0)
  
  term1 = ifelse(term1<0,0,term1)
  term2 = ifelse(term2<0,0,term2)
  
  
  res = -sum(  y*log(term1)+(1-y)*log(term2)   )
  return(res)
}
gr_joint = function(par,x,y){
  beta0 = par[1]
  beta1 = par[-c(1,length(par))]
  
  c1 = par[length(par)]
  
  var1 = sigma(x%*%beta1+beta0)*(1-sigma(x%*%beta1+beta0))
  sigma1 = sigma(x%*%beta1+beta0)
  
  a= var1*( (-y+c1*sigma1)/(sigma1*(1-c1*sigma1)) )
  res = t(cbind(1,x))%*%a
  
  gr_c1 = sum(-y/c1 + (1-y)*sigma1/(1-c1*sigma1))
  res = c(res,gr_c1)
  
  return(res)
}


logistic_fit_joint = function(x,y){
  
  optim1 = optim(par=c(rep(0,ncol(x)+1),0.5),fn=logLike_joint,gr=gr_joint,x=x,y=y,method="BFGS")
  par = optim1$par
  return(list(par=par))
}