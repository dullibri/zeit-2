#-------------------------------------------------------------------------------																
# This code is designed to create a detailed list of Bundesbank variables to be 
# downloaded from its webpage:
# http://www.bundesbank.de/Navigation/DE/Statistiken/Zeitreihen_Datenbanken/Makrooekonomische_Zeitreihen/makrooekonomische_zeitreihen_node.html

																			
# Written by K.A.Kholodilin												
# DIW Berlin
# kkholodilin (at) diw . de
	
# First created --- February 28, 2015										
# Last modified --- February 28, 2015
#-------------------------------------------------------------------------------

rm(list = ls())

Begin=proc.time()

library(R.oo)
library(XML)

#-------------------------------------------------------------------------------
#--- Define the paths

sFolder = "d:/users/KKholodilin/MyCodes/BuBa/Data/"
sFolder = "I:/Personal_Folders/employees/KX/BuBa/Data/"
sInFile = "Buba_Variables_List.csv"
sOutFile = "Buba_Detailed_Variables_List.csv"

#-------------------------------------------------------------------------------
#--- Load list of variables

X = read.csv(paste(sFolder, sInFile, sep=""))
NVar = nrow(X)

#-------------------------------------------------------------------------------
#--- Download links

NVar = 100

Z = c()

  for(i in 1:NVar)
  {
sInFile_i = as.character(X$Link[i])
Z_i = readHTMLTable(sInFile_i, encoding="UTF-8")
Z_i = Z_i$`NULL` 
Z_i$Herunterladen = NULL 
Z_i$Datenkorb = NULL
Z_i$Zeitreihe = unlist(lapply(strsplit(as.character(Z_i$Zeitreihe), "\n"), function(x) x[1]))

Z = rbind(Z, Z_i)

print(paste(i, "out of", NVar));
  }

#-------------------------------------------------------------------------------
#--- Save output

write.csv(Z, paste(sFolder, sOutFile, sep=""), row.names=F)

#-------------------------------------------------------------------------------
proc.time() - Begin
