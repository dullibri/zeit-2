#-------------------------------------------------------------------------------																
# This code is designed to download the data on cost of living from an Internet 
# page http://www.numbeo.com.

																			
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
sDate = format(Sys.Date(), format="%Y_%m")
sFolder = "d:/users/KKholodilin/MyCodes/BuBa/Data/"
sFolder = "I:/Personal_Folders/employees/KX/BuBa/Data/"
#sInFile = "http://www.bundesbank.de/Navigation/DE/Statistiken/Zeitreihen_Datenbanken/Makrooekonomische_Zeitreihen/makrooekonomische_zeitreihen_node.html?openAll=true"
sInFile = "http://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Macro_economic_time_series/macro_economic_time_series_node.html?openAll=true"
sOutFile = paste("Buba_Variables_List_", sLang, ".csv", sep="")

#-------------------------------------------------------------------------------
#--- Download links

X = readLines(sInFile, warn=F, encoding="UTF-8")
X = gsub("\"", "", X)

Sel = grep("class=node open", X)
Sel = grep("listId=", X)
Aux = X[Sel]
Link = unlist(lapply(strsplit(as.character(Aux), "href="), function(x) x[2]))
Name = unlist(lapply(strsplit(as.character(Link), "title="), function(x) x[2]))
Name = unlist(lapply(strsplit(as.character(Name), ">"), function(x) x[1]))
Link = unlist(lapply(strsplit(as.character(Link), " title="), function(x) x[1]))

Y = data.frame(Link, Name)

#-------------------------------------------------------------------------------
#--- Save output

write.csv(Y, paste(sFolder, sOutFile, sep=""), row.names=F)

#-------------------------------------------------------------------------------
proc.time() - Begin
