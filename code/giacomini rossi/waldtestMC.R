## This file simulates the DGP of MC1 in section 7 of Giacomini and Rossi
# (2009, Review of Economic Studies) and performs the forecast breakdown
# test, and a Wald Test on predicting the surprise losses by an AR-process.
# The confidence interval of fitted surprise losses is also computed to
# date the occurence of forecast breakdowns.

## Settings
# Parameters for MC DGP
beta = c(2.73,-0.44)
alphaeps = 0

schemelist = c("fixed","rolling","recursive")

# Maximum lag length in AR of SL
pmax = 12

# Confidence level for Wald Test CI
alpha = 0.05

# HAC estimation of variance-covariance matrix [1 if true, 0 otherwise]
hacestim = 0

# Assuming conditional homoskedasticity in regression residuals
condhomosk = 1

## Sample length
# Training sample length
m = 50
# Evaluation period
n = 150
# Total sample length
T = m+n

# Forecast horizon
tau = 1

# Bandwidth for HAC estimator [0 or n^(1/3)]
bw = 0
if (hacestim==1){bw = round(n^(1/3))}

## Selection of estimation scheme
i = 2
# Estimation scheme
scheme = schemelist[i]

## Start simulation
seedsim = 1

## Simulate series
set.seed(seedsim)
# Regressors
x = matrix(rnorm(T,0,1),ncol=1)
# Disturbances
u = matrix(rnorm(T,0,1),ncol=1)
sigma = matrix(NA,T,1)
eps = matrix(NA,T+1,1)
eps[1] = 0
for (j in 1:T){
  sigma[j] = sqrt(1+alphaeps*(eps[j]^2))
  eps[j+1] = sigma[j]*u[j];
}
eps = eps[2:(T+1),]
# DGP
y = cbind(matrix(1,T,1),x)%*%beta+eps

# Inputs to the function
LO = matrix(NA,n,1)
LI = matrix(NA,n,1)

## Forecast breakdown test inputs
for (s in 1:n){
  if (scheme=="fixed"){
  Yest = y[1:m]
  Xest = cbind(matrix(1,m,1),x[1:m])
  }
if (scheme=="rolling"){
  Yest = y[s:(m+s-1)]
  Xest = cbind(matrix(1,m,1),x[s:(m+s-1)])
  }
if (scheme=="recursive"){
  Yest = y[1:(m+s-1)]
  Xest = cbind(matrix(1,m+s-1,1),x[1:(m+s-1)])
  }
  beta = solve(t(Xest)%*%Xest)%*%t(Xest)%*%Yest
  Yfit = Xest%*%beta
  uhat = Yfit-Yest
  LI[s] = mean(uhat^2)
  yfc = cbind(1,x[m+s])%*%beta
  ehat = yfc-y[m+s]
  LO[s] = ehat^2
}
## Forecast breakdown test
resultsFB = grtest(LO,LI,Yest,Xest,scheme,bw)
SL = resultsFB[[1]]
SLbar = resultsFB[[2]]
tstatFBc = resultsFB[[3]]
pvalFBc = resultsFB[[4]]
tstatFB = resultsFB[[5]]
pvalFB = resultsFB[[6]]
sigma2 = resultsFB[[7]]
        
## Predicting future breakdowns and Wald Test

# Regressor matrix Z
# Find opimal lag length pstar
BIC = matrix(NA,pmax,1)
for (p in 1:pmax){
resultsOLSAR = olsself(SL,p)
BIC[p] = resultsOLSAR[[3]]
}
pstar = which.min(BIC)

resultsOLSAR = olsself(SL,pstar)
deltahat = resultsOLSAR[[1]]
reshat = resultsOLSAR[[2]]
Z = resultsOLSAR[[4]]
SLhat = SL[(pstar+1):n]

# Fitted surprise losses
SLfit = Z%*%deltahat

# Wald test statistic and CI
resultsWaldCI = FB_Wald_CI(Z,reshat,deltahat,n,m,sigma2,condhomosk,scheme,bw,alpha)
Wstat = resultsWaldCI[[1]]
pvalW = resultsWaldCI[[2]]
SLfitCI = resultsWaldCI[[3]]


plot(1:149,SLfit,type='l')
lines(1:149,SLfitCI,type='l',col="red")
