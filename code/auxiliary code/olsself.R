olsself=function(y,p){
## Header
# This function runs an OLS estimation of an autoregressive model with K
#
# Inputs:
# y: Tx1 time series of level variable y
# p: Number of lags to be included of difference of y
#
# Output:
# b: (p+1)x1 vector of estimated parameters
# res: (T-p)x1 vector of residuals
# BIC: Bayesian Information Criterion for p lags.
# Z: (T-p)x(p+1) matrix of regressors

## Function
y = as.matrix(y)
T = nrow(y)

Z = matrix(NA,T-p,p)
for (i in 1:p){
Z[,i] = y[(p+1-i):(T-i)]
}
Z = cbind(matrix(1,T-p,1),Z)
y = y[(p+1):T]

# OLS estimator
b = solve(t(Z)%*%Z)%*%t(Z)%*%y
yfit = Z%*%b
res = y-yfit
sigma_u = t(res)%*%res/(T-p-2)
# Information criteria for determining the optimal lag length
BIC = T*log(sigma_u)+(p+1)*log(T)

results = list(b=b,res=res,BIC=BIC,Z=Z,yfit=yfit)
return(results)
}