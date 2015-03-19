f_Clark_West_Test = function(vFE_small, vFE_big, vFcst_small, vFcst_big, cH)
{
#--- Compute Clark-West test suggested in 
# Clark, Todd E. & West, Kenneth D. (2007)
# "Approximately normal tests for equal predictive accuracy in nested models"
# Journal of Econometrics, vol. 138(1), pages 291-311.

cP = length(vFE_small) - cH + 1 # Number of degrees of freedom
vCW = vFE_small^2 - vFE_big^2 + (vFcst_small - vFcst_big)^2 # Forecast differential with adjustment (last term)
cCW_bar = mean(vCW); # Average forecast differential

Bandwidth = round(1.5 * cH)
NW =  f_Newey_West_vector((vCW - cCW_bar), Bandwidth) # Newey-West variance of forecast differential
cNWcw = NW[[1]]
cStat = cCW_bar/(sqrt(cNWcw)/sqrt(cP)) # Clark-West test statistic
cPVal = 1 - pnorm(cStat) # p-value of the test statistic

#print(sqrt(cNWcw))
# print(cCW_bar/sqrt(cNWcw))
# print(c(sqrt(cP), sqrt(length(vFE_small))))
# print(Bandwidth)

Out = list(cStat, cPVal)
#  return meanc(vFEsmall.^2)~meanc(vFEbig.^2)~meanc((vFcstSmall - vFcstBig).^2)~cCWbar~cStat~cPval;
}