f_Newey_West_bootstrapable = function(mU, Inv_Block_Param)
{
#   /*************   Newey-West (Boriss Siliverstovs, April 2006) *************/
#     /*                                                                        */
#     /*                       NEWEY - WEST covariance matrix                   */
#     /*                                                                        */
#     /**************************************************************************/
#     /*
#     author: Boriss Siliverstovs
#   DIW Berlin
#   and 
#   Konstantin A. Kholodilin
#   DIW Berlin
#   
#   correspondence to: bsiliverstovs (AT) diw.de
#   /**************************************************************************/
#     
#     Notes:
#     
#     The arguments are:
#     
#   - a matrix (T x k) of a time series, k = 1,2,...
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
cLag = NRow - 1

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
#--- Compute weights for autocovariances 
vLag = c(1:cLag)
vWeight = (1 - vLag/cLag) * (1 - Inv_Block_Param)^vLag 
+ (vLag/cLag)* (1 - Inv_Block_Param)^(cLag - vLag)
# print(mU);
#--- Compute autocovariances and accumulate them
  for(i in 1:cLag)
  {# i=1
Aux = cbind(mU, amLagsU[[i]])
Aux = Aux[complete.cases(data.frame(Aux)),,drop=F]
if(nrow(Aux)==1){mACV = sum(Aux[1]*Aux[2]) / NRow}else{
mACV = sum(apply(Aux, 1, prod)) / NRow}
mNeweyWest = mNeweyWest + 2*mACV*vWeight[i]	#
mACVall = mACVall + mACV * vWeight[i];
  } # end of loop i

return(mNeweyWest)  
}