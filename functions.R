sigma = function(s) {
  res = exp(s) / (1 + exp(s))
  return(res)
}

scale01 = function(x){
  if(min(x)>=0){
    x=x-min(x)
    x=x/max(x)
  }else if(min(x)<0){
    x = x+abs(min(x))
    x=x/max(x)
  }
  return(x) 
}  