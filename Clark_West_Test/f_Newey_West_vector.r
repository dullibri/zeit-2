f_Newey_West_vector = function(mU, cLag)
{
#   /*************   Newey-West (Boriss Siliverstovs, April 2006) *************/
#     /*                                                                        */
#     /*                       NEWEY - WEST covariance matrix                   */
#     /*                                                                        */
#     /**************************************************************************/
#     /*
#     author: Boriss Siliverstovs
#   DIW Berlin
#   
#   correspondence to: bsiliverstovs (AT) diw.de
#   /**************************************************************************/
#     
#     Notes:
#     
#     The arguments are:
#     
#     - a matrix (T x k) of a time series, k = 1,2,...
#   - an integer lag-truncation ("lag");
#   
#   The procedures returns a matrix (k x k), with
#   
#   - NEWEY - WEST covariance matrix;
#   - Correlogram, could be used for FM-OLS estimation
#   of cointegrating vector.
#   
#   This program is for public, non commercial use.
#   Nevertheless, the author disclaims any responsibility for its use.
#   */
#     /**************************************************************************/
    
NCol = 1
NRow = length(mU)

amLagsU = list() 

#--- Constructing lags
  for(i in 1:cLag)
  {
mNA = rep(NA, i)
amLagsU[[i]] = c(mNA, mU[1:(NRow-i)])   
  }
  
#--- Variance
mVarU = sum(mU^2) / NRow    

mNeweyWest = mVarU;
mACVall = 0 
vWeight = 1 - c(1:cLag)/(1+cLag) # Compute weights for autocovariances 

#--- Compute autocovariances and accumulate them
  for(i in 1:cLag)
  {
Aux = cbind(mU, amLagsU[[i]])
vSel_NA = which(is.na(rowSums(Aux)==T))
Aux = Aux[-vSel_NA,]
mACV = sum(apply(Aux, 1, prod)) / NRow
mNeweyWest = mNeweyWest + 2*mACV*vWeight[i]	#
mACVall = mACVall + mACV * vWeight[i];
  } # end of loop i

Output = list(mNeweyWest, mACVall)  
}