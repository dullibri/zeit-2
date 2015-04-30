#-------------------------------------------------------------------------------																
# This code is designed to conduct White test for data snooping.
#
# The null hypothesis: no predictive superiority over the benchmark model.

																			
# Written by K.A.Kholodilin												
# DIW Berlin
# kkholodilin (at) diw . de
	
# First created --- April 15, 2015  									
# Last modified --- April 15, 2015
#-------------------------------------------------------------------------------

rm(list = ls())

Begin=proc.time()

library(R.oo)

source("I:/Personal_Folders/employees/KX/A_MTI/Code/Hansen_2005_test/f_Hansen_2005_Reality_Check.r") 
source("I:/Personal_Folders/employees/KX/A_MTI/Code/Hansen_2005_test/f_Newey_West_bootstrapable.r") 
source("I:/Personal_Folders/employees/KX/A_MTI/Code/White_test/f_Politis_Romano_Bootstrap.r") 

#-------------------------------------------------------------------------------
#--- Initial settings

NRep = 1000
sFolder = "I:/Personal_Folders/employees/KX/A_MTI/Daten/"
sInFile = "fe_test.csv"

#-------------------------------------------------------------------------------
#--- Load the forecast errors

X = read.csv(paste(sFolder, sInFile, sep=""))

#-------------------------------------------------------------------------------
#--- Transpose data set

svVar = as.character(X$X)
NVar = length(svVar)
Y = c()
  for(i in svVar)
  {
Y_i = X[which(X$X==i),]
Y_i = Y_i[-1]
Y_i = as.double(Y_i)
Y = cbind(Y, Y_i)
  }
Y = data.frame(Y)
names(Y) = svVar

#-------------------------------------------------------------------------------
#--- Carry out test

Sel_bench = which(names(Y) == "ar")
benchmark = Y[, Sel_bench]
dim(benchmark) = c(nrow(Y), 1)
alternative = as.matrix(Y[, -Sel_bench])
dim(alternative) = c(nrow(Y), (ncol(Y)-1))
Block_Param = nrow(Y) # Bootstrap block size

Out_MSE = f_Hansen_2005_Reality_Check(alternative, 1, benchmark, NRep, 0, Block_Param)
Out_MAE = f_Hansen_2005_Reality_Check(alternative, 3, benchmark, NRep, 0, Block_Param)
PValue = Out_MSE[[1]]
PValue = rbind(PValue, Out_MAE[[1]])
Model = c("MSE", "MAE")
Test = data.frame(Model, PValue)
row.names(Test) = NULL

#-------------------------------------------------------------------------------

proc.time() - Begin
