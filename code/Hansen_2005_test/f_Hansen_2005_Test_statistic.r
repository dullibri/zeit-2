f_Hansen_2005_Test_statistic = function(loss, blockparam)
{
# This function computes test statistic for Hansen (2005) superior predictive
#  ability test.
#
# Converted to R by Konstantin A. Kholodilin
# DIW Berlin
# kkholodilin (at) diw . de

# First created --- April 15, 2015										
# Last modified --- April 28, 2015
#-------------------------------------------------------------------------------

# library(pracma) # for repmat
# library(matrixStats) # For rowMax

NRow = length(loss)

droof = mean(loss)

Omega_hat = f_Newey_West_bootstrapable(loss, blockparam)

T_SPA = sqrt(NRow) * droof / Omega_hat
out=c(T_SPA,Omega_hat)
return(out)
}