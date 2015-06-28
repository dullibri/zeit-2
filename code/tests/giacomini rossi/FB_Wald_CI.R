FB_Wald_CI = function(Z,reshat,deltahat,n,m,sigma2,condhomosk=1,scheme,bw,alpha=0.05){

## Description ##
# ----------------------------------------------------------------------- #
# This function conducts the Wald test on predicting surprise losses by a
# set of explanatory variables and analysing their behavior over time by 
# confidence intervals as proposed by Giacomini & Rossi (2009, The Review
# of Economic Studies).
# ----------------------------------------------------------------------- #
# INPUT to the function:
# Z:          [(tz)x(k)] Matrix of predictors
# reshat:     [tz] Vector of regression residuals
# deltahat:   [k] vector of paramater estimates
# n:          Evaluation window size
# m:          Training sample length for estimation
# sigma2:     Variance of out-of-sample surprise losses
# condhomosk: Indicator for assumption of conditional homoskedasticity in
#             regression residuals (1 if true (default))
# scheme:     String sececting the forecasting scheme
#             'fixed', 'rolling', 'recursive' (default)
# bw:         Bandwidth for HAC estimator (n^(1/3) as default)
# alpha:      Confidence level for CI (0.05 as default)
# ----------------------------------------------------------------------- #
# OUTPUT of the function:
# Wstat:   Wald test statistic
# pvalW:   p-value for Wald test statistic
# SLfitCI: [tz] vector providing the CI for fitted surprise losses
# ----------------------------------------------------------------------- #
  
# Scaling parameter of covariance matrix for Wald Test
pie = n/m
LAMBDA = 1
if (scheme=='recursive'){LAMBDA = log(1+pie)/pie}
if (scheme=='rolling' & n<=m){LAMBDA = 1-pie/2}
if (scheme=='rolling'& n>m){LAMBDA = (2*pie)^(-1)}

# Demeaned regressors
tz = nrow(Z)
k = ncol(Z)
ktilde = k-1
Ztilde = Z[,2:k]
Ztilde = Ztilde-mean(Ztilde)

# Inputs to covariance matrix for Wald Test
Szz = t(Ztilde)%*%Ztilde/tz
SzLL = t(Ztilde)%*%reshat^2/tz
SzLzL = t(Ztilde*(reshat%*%matrix(1,1,ktilde)))%*%(Ztilde*(reshat%*%matrix(1,1,ktilde)))/tz

if (bw>0){
  for (j in 1:bw){
    #     GAMMAzz = t(Ztilde[(1+j):tz,])%*%Ztilde[1:(tz-j),]/tz
    #     Szz = Szz + (1-j/(bw+1))*(GAMMAzz+t(GAMMAzz))
    GAMMAzLL = (t(Ztilde[(1+j):tz,])%*%(reshat[(1+j):tz]*reshat[1:(tz-j)])+t(Ztilde[1:(tz-j),])%*%(reshat[1:(tz-j)]*reshat[(1+j):tz]))/tz
    SzLL = SzLL + (1-j/(bw+1))*GAMMAzLL
    GAMMAzLzL = t(Ztilde[(1+j):tz,]*(reshat[(1+j):tz]%*%matrix(1,1,ktilde)))%*%(reshat[1:(tz-j)]%*%matrix(1,1,ktilde)*Ztilde[1:(tz-j),])/tz;
    SzLzL = SzLzL + (1-j/(bw+1))*(GAMMAzLzL+t(GAMMAzLzL))
  }
}
if (k>2){
  A = rbind(cbind(1, -colMeans(Z[,2:k])%*%solve(Szz)),cbind(matrix(0,ktilde,1), solve(Szz)))
} else {
  A = rbind(cbind(1, -mean(Z[,2])%*%solve(Szz)),cbind(matrix(0,ktilde,1), solve(Szz)))
}
C = rbind(cbind(sigma2, LAMBDA*t(SzLL)),cbind(LAMBDA*SzLL, SzLzL))

# Covariance matrix for Wald Test
Omegahat = A*C*t(A)
if (condhomosk==1){Omegahat = A*diag(diag(C))*t(A)}

# Test statistic and p-value
Wstat = n*t(deltahat)%*%solve(Omegahat)%*%deltahat
pvalW = 1-pchisq(Wstat,k)

# Confidence interval for fitted surprise losses
SLfitCI = Z%*%deltahat-qnorm(1-alpha)*sqrt(diag((Z%*%Omegahat%*%t(Z)/n)));

results = list(Wstat=Wstat,pvalW=pvalW,SLfitCI=SLfitCI)
return(results)
}