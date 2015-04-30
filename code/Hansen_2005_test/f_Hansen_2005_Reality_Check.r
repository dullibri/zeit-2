# A reality check for data snooping by Halbert White, which uses 
# the stationary bootstrap by Politis & Romano.

f_Hansen_2005_Reality_Check = function(alternative, flag, benchmark, n, display, blockparam)
{
#  This file contains the White reality check for data snooping
#   -   This can be used to test whether atleast one of the created models
#   outperforms a benchmark model  
#   - This can also be used to test whether you have found atleast one profitable
#   trading strategy, or test whether you have found atleast one trading
#   strategy that outperforms the benchmark.
# 
# 
#   The H0 hypothesis is always that you have not found an outperforming
#   strategy or model.
# 
#  Input:
#    'Alternative' is a matrix with  Returns ==> [-1 , +Inf ]
#   or residuals of predictions with a model ==> [ -Inf ; +Inf]
#  Important, the rows stand for new observation the columns for the models
#     'Flag'  indicates which loss function you want to use
#   Flag = 1 test for model superiority vs benchmark by mean squared error
#   Flag = 2 test for trading return superiorty vs benchmark 
#   Flag = 3 test for model superiority vs benchmark by absolute error
#     'Benchmark' contains a vector with - (1) The residuals from the predictions
#  of the benchmark model, (2) The returns of your benchmark trading
#  strategy, (3) Special case where Benchmark is the single number 0, where
#  your benchmark is zero.
#     'n' is the number of bootstrapped series you want to create aka the
#   number of simulations. In academic papers this is usually set 
#   to atleast 500 for reasonable results
#     'display' is a number [0, 1] if 0, then display is set off, if 1, then
#  display is on.
# 
#  Examples of possibles testing can be found in help.txt file 
# 
# by Arnout Tilgenkamp
#   
# 21 Dec 2011 (Updated 15 Oct 2013)
# http://www.mathworks.com/matlabcentral/fileexchange/34306-white-reality-check/content/WhiteRealityCheck.m
#
#
# Converted to R by Konstantin A. Kholodilin
# DIW Berlin
# kkholodilin (at) diw . de

# First created --- April 15, 2015										
# Last modified --- April 15, 2015
#-------------------------------------------------------------------------------

library(pracma) # for repmat
library(matrixStats) # For rowMax

NRow = nrow(alternative)
NCol = ncol(alternative)

#  condition whether you have a benchmark or you want to test vs 0
  if(benchmark != 0|| length(benchmark)>1)
  {
mat = repmat(benchmark, 1, NCol) # In original code, c instead of NCol, but not clear, what it is
  }else{
mat = benchmark # mat = 0 benchmark is zero
  }

#  loss functions 
  if(flag == 1) {f = - alternative^2 + mat^2}
  if(flag == 2) {f = log(1+alternative) - log(1+mat)}
  if(flag == 3) {f = - abs(alternative) + abs(mat)}

# input for average block size for P&R bootstrap (geometric distribution)
# input = inputdlg('What do you want as average block size for the Politis Romano stationary bootstrap?');
# blockparam = 1/(str2num(input{1})+1);
blockparam = 1 / blockparam

droof = colMeans(f)

# Actual Politis Romano bootstrap as described in Politis and Romano (1994)
Z_star =  f_Politis_Romano_Bootstrap(alternative, n, blockparam, display, flag, mat)
Z_roof_star = colMeans(Z_star)

Omega_hat = rep(NA, NCol)
vT_SPA = rep(NA, NCol)
vT_SPA_star = rep(NA, NCol)
  for(i in 1:NCol)
  {
Omega_hat[i] = f_Newey_West_bootstrapable(f[,i], blockparam)[[1]]
# print(Omega_hat[i]);
# print(var(f[,i], na.rm=T));
vT_SPA[i] = sqrt(NRow)*droof[i]/Omega_hat[i]
vT_SPA_star[i] = sqrt(NRow)*Z_roof_star[i]/Omega_hat[i]
  }

T_SPA_n = max(max(vT_SPA), 0)
T_SPA_star_bn = max(max(vT_SPA_star), 0)

#--- Compute p-values
better = T_SPA_star_bn > T_SPA_n

pvalue = sum(better)/n

# compare T_SPA and T*_SPA to get the p-value
Out = list(pvalue, T_SPA_star_bn, T_SPA_n)

return(Out)
}