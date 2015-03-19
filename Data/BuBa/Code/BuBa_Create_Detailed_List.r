#-------------------------------------------------------------------------------																
# This code is designed to create a detailed list of Bundesbank variables to be 
# downloaded from its webpage:
# http://www.bundesbank.de/Navigation/DE/Statistiken/Zeitreihen_Datenbanken/Makrooekonomische_Zeitreihen/makrooekonomische_zeitreihen_node.html

																			
# Written by K.A.Kholodilin												
# DIW Berlin
# kkholodilin (at) diw . de
	
# First created --- February 28, 2015										
# Last modified --- March 3, 2015
#-------------------------------------------------------------------------------

rm(list = ls())

Begin=proc.time()

library(R.oo)

#-------------------------------------------------------------------------------
#--- Define the paths

sLang = "EN" # "DE"
sFolder = "d:/users/KKholodilin/MyCodes/BuBa/Data/"
sFolder = "I:/Personal_Folders/employees/KX/BuBa/Data/"
sInFile = paste("Buba_Variables_List_", sLang, ".csv", sep="")
sOutFile = paste("Buba_Detailed_Variables_List_", sLang, ".csv", sep="")

#-------------------------------------------------------------------------------
#--- Load list of variables

X = read.csv(paste(sFolder, sInFile, sep=""))
NVar = nrow(X)

#-------------------------------------------------------------------------------
#--- Download links

#NVar = 10

Z = c()

  for(i in 1:NVar)
  {
sInFile_i = as.character(X$Link[i])
Y = readLines(sInFile_i, warn=F, encoding="UTF-8")
Y = gsub("\"", "", Y)    

Sel = grep("its_fileFormat=", Y)
Sel = Sel[seq(1, length(Sel), 2)]
Aux = Y[Sel]

Code = unlist(lapply(strsplit(as.character(Aux), "tsId="), function(x) x[2]))
Code = unlist(lapply(strsplit(as.character(Code), "&"), function(x) x[1]))

Variable = Y[Sel-2]
Variable = gsub("<td>", "", Variable)
Variable = gsub("</td>", "", Variable)
Variable = trim(Variable)

Z_i = data.frame(Code, Variable)
Z = rbind(Z, Z_i)

print(paste(i, "out of", NVar));
  }

#-------------------------------------------------------------------------------
#--- Save output

write.csv(Z, paste(sFolder, sOutFile, sep=""), row.names=F)

#-------------------------------------------------------------------------------
proc.time() - Begin
