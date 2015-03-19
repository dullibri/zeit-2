#-------------------------------------------------------------------------------																
# This code is designed to conduct Clark-West test of equal prediction accuracy.

																			
# Written by K.A.Kholodilin												
# DIW Berlin
# kkholodilin (at) diw . de
	
# First created --- March 10, 2015										
# Last modified --- March 10, 2015
#-------------------------------------------------------------------------------

rm(list = ls())

Begin=proc.time()

library(R.oo)
#library(car)
#library(stargazer)
source("I:/Personal_Folders/employees/KX/A_MTI/Code/CMC_Test/f_Newey_West_vector.r") 
source("I:/Personal_Folders/employees/KX/A_MTI/Code/CMC_Test/f_Clark_West_Test.r") 

#-------------------------------------------------------------------------------
#--- Initial settings

cH = 12
NObs = 300
sFolder = "h:/kkholodilin/MyCodes/Immo_Hist/"
sFolder = "I:/Personal_Folders/employees/KX/A_MTI/"
# sInFile = paste("Data/Immobilien_Anzeigen_", sDate, "_Ortsteile.csv", sep="") # "Immobilien_Anzeigen_1914-1915_Coord_Google_maps.csv"
# sOutFile = paste("Data/Hedonic_OLS_frequencies_by_districts_", sDate, ".csv", sep="")

#-------------------------------------------------------------------------------
#--- Generate data

Act_0 = 2 + rnorm(1)
vEps = rnorm(NObs)
vAct = c(Act_0, rep(0, NObs))
  for(i in 2:(NObs+1))
  {
vAct[i] = 2 + 0.9 * vAct[i-1] + vEps[i-1]    
  }
#vAct = rnorm(NObs)
vAct = vAct[-1]
vFcst_small = rnorm(NObs)
vFcst_big = rnorm(NObs)
vFcst_small = vAct - .05 + rnorm(NObs) / 1.5
vFcst_big = vAct + rnorm(NObs) / 1.5
vFcst_big = vFcst_small + rnorm(NObs) / 10
vFE_small = vAct - vFcst_small
vFE_big = vAct - vFcst_big

MSE_small = sum(vFE_small^2) / length(vFE_small)
MSE_big = sum(vFE_big^2) / length(vFE_big)
Tail_U = MSE_big / MSE_small

#-------------------------------------------------------------------------------
#--- Plot actuals, forecasts, and forecast errors

par(mfrow=c(2,1))
plot(vAct)
lines(vFcst_big)
lines(vFcst_small, col="red")

plot(vFE_small, type="l", col="red")
lines(vFE_big)

#-------------------------------------------------------------------------------
#--- Conduct test

Test = f_Clark_West_Test(vFE_small, vFE_big, vFcst_small, vFcst_big, cH)
Test
Tail_U

#-------------------------------------------------------------------------------

proc.time() - Begin
