rm(list = ls())

source("logistic_fit_joint.R")
source("functions.R")

#SET PARAMETERS:
c = 0.7 #label frequency
b = 1 #true signal
p = 10 #number of features
n = 10000 #sample size

#GENERATE ARTIFICIAL DATASET:
beta = b*rep(1,p)
beta0 = 1
x = matrix(0, nrow = n, ncol = p)
for (j in 1:p) {
  x[, j] = rnorm(n, 0, 1)
}
y = numeric(n)
for (i in 1:n) {
  lc = beta0+sum(x[i, ] * beta)
  prob = sigma(lc)
  y[i] = rbinom(1, 1, prob)
}

#CREATE SURROGATE VARIABLE S:
s = numeric(n)
for (i in 1:n) {
  if (y[i] == 1) {
    s[i] = rbinom(1, 1, c)
  }
}
beta_true = c(beta0,beta)



#ORACLE METHOD:
model_oracle = glm(y ~ x, family = "binomial")
beta_oracle = as.numeric(coef(model_oracle))


#NAIVE METHOD:
model_naive = glm(s ~ x, family = "binomial")
beta_naive = as.numeric(coef(model_naive))


#JOINT METHOD:
par_joint = logistic_fit_joint(x, s)$par
beta_joint = par_joint[-length(par_joint)]
c_joint = par_joint[length(par_joint)]


#COMPARE THE RESULTS:
cat("Parameters (true):",beta_true,"\n")
cat("Parameters (oracle):",beta_oracle,"\n")
cat("Parameters (naive):",beta_naive,"\n")
cat("Parameters (joint method):",beta_joint,"\n")
cat("Estimated c (joint method):" ,c_joint,"\n")



