#-------------------------------------------------------------------------------																
# This code is designed to download statistical data from Deutsche Bundesbank's  
# webpage:
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

#-------------------------------------------------------------------------------
#--- Define the paths

sDate = format(Sys.Date(), format="%Y_%m")
sFolder = "d:/users/KKholodilin/MyCodes/BuBa/Data/"
sFolder = "I:/Personal_Folders/employees/KX/BuBa/Data/"
sFolder_out = paste(sFolder, "BuBa_", sDate, "/", sep="")
sInFile = "Buba_Detailed_Variables_List.csv"

#-------------------------------------------------------------------------------
#--- Load list of variables

X = read.csv(paste(sFolder, sInFile, sep=""))
NVar = nrow(X)

#-------------------------------------------------------------------------------
#--- Download links

dir.create(sFolder_out, showWarnings=F)

#NVar = 6

  for(i in 1:NVar)
  {
Code = as.character(X$Code[i])
#sInFile_i = paste("http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=", Code, "&its_csvFormat=de&its_fileFormat=csv&mode=its", sep="")
sInFile_i = paste("http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=", Code, "&mode=its&its_fileFormat=csv&its_csvFormat=en&its_currency=hypothetical&its_dateFormat=default&its_from=&its_to=", sep="")

sOutFile_i = paste(sFolder_out, Code, ".csv", sep="")
download.file(sInFile_i, sOutFile_i, quiet = T, mode = "wb")

print(paste(i, "out of", NVar));

  }

#-------------------------------------------------------------------------------
proc.time() - Begin
