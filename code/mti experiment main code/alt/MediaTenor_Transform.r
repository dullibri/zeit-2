#-------------------------------------------------------------------------------																
# This code is designed to analyze the media data on economic situation.

																			
# Written by K.A.Kholodilin												
# DIW Berlin
# kkholodilin (at) diw . de
	
# First created --- April 16, 2014										
# Last modified --- April 16, 2014
#-------------------------------------------------------------------------------

rm(list = ls())

Begin=proc.time()

memory.limit(size=4000)

#-------------------------------------------------------------------------------
#--- Initial settings

sFolder = "d:/users/kkholodilin/MyCodes/MDR/Data/MediaTenor/" 
sFolder = 'H:/git/zeit-2/Data/Mediatenor/'
sInFile = "WiLage_lang_2001m01-2015m01_MTI.csv"

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
fACF = function(Data1, Data2, MaxLag)
{
vCor = rep(NA, (2*MaxLag+1))  

vCor[MaxLag+1] = cor.test(Data1, Data2)$estimate

  for(k in 1:MaxLag)
  {
Series1 = Data1[1:(length(Data1)-k)]
Series2 = Data2[k:length(Data2)]
vCor[k] = cor.test(Series1, Series2)$estimate
  }
vCor
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#--- Loading the data

X = read.csv(paste(sFolder, sInFile, sep=""))
X = X[grep("Deut", X$Raumbezug),]

#-------------------------------------------------------------------------------
#--- Recoding evaluation variable

X$Eval = 0
X$Eval[which(X$Bewertung=="positiv")] = 1
X$Eval[which(X$Bewertung=="negativ")] = -1
summary(X)

#-------------------------------------------------------------------------------
#--- Computing daily balances

svDate = seq(as.Date("2001/1/1"), as.Date("2014/4/16"), "days")
Year = unlist(lapply(strsplit(as.character(svDate), "-"), function(x) x[1]))
Month = unlist(lapply(strsplit(as.character(svDate), "-"), function(x) x[2]))
Day = unlist(lapply(strsplit(as.character(svDate), "-"), function(x) x[3]))
Y = data.frame(Year, Month, Day)
Y$Date = paste(Day, Month, Year, sep=".")
Y$Year_Month = paste(Year, Month, sep="_")

Aux = table(X$Datum, X$Eval)
vMatch = match(row.names(Aux), Y$Date)

svName = c("neg", "neu", "pos")
svName_all = paste("All", svName, sep="_")
Y[,svName_all] = NA
Y[vMatch, svName_all] = Aux

#--- Past

X_past = subset(X, Zeitbezug=="Vergangenheit")
svName_past = paste("Past", svName, sep="_")
Aux = table(X_past$Datum, X_past$Eval)
Y[,svName_past] = NA
Y[vMatch, svName_past] = Aux

#--- Present

X_pres = subset(X, Zeitbezug=="Gegenwart")
svName_pres = paste("Pres", svName, sep="_")
Aux = table(X_pres$Datum, X_pres$Eval)
Y[,svName_pres] = NA
Y[vMatch, svName_pres] = Aux

#--- Future

X_fut = subset(X, Zeitbezug=="Zukunft")
svName_fut = paste("Fut", svName, sep="_")
Aux = table(X_fut$Datum, X_fut$Eval)
Y[,svName_fut] = NA
Y[vMatch, svName_fut] = Aux

#--- Computing volatility

Y$Index_All = 100 * (Y$All_pos - Y$All_neg) / (Y$All_pos + Y$All_neg + Y$All_neu)
Y$Index_Pres = 100 * (Y$Pres_pos - Y$Pres_neg) / (Y$Pres_pos + Y$Pres_neg + Y$Pres_neu)
Y$Index_Fut = 100 * (Y$Fut_pos - Y$Fut_neg) / (Y$Fut_pos + Y$Fut_neg + Y$Fut_neu)

#-------------------------------------------------------------------------------
#--- Saving output

NObs = nrow(Y)
sBegDate = paste(Y$Year[1], Y$Month[1], Y$Day[1], sep="-")
sEndDate = paste(Y$Year[NObs], Y$Month[NObs], Y$Day[NObs], sep="-")
sOutFile1 = paste("MediaTenor_", sBegDate, "_", sEndDate, ".csv", sep="")

write.csv(Y, paste(sFolder, sOutFile1, sep=""), na="", row.names=F)

#-------------------------------------------------------------------------------
#--- Aggregating data to monthly frequency

svVar = names(Y)
svVar = setdiff(svVar, c("Year", "Month", "Day", "Date", "Year_Month"))
svVar = svVar[-grep("Index", svVar)]

vYear = c(2001:2014)
NYear = length(vYear)
Year = rep(vYear, each=12)
Month = rep(c(1:12), NYear)
Z = data.frame(Year, Month)
sMonth = as.character(Month)
sMonth[which(Month<10)] = paste(0, Month[which(Month<10)], sep="")
Z$Year_Month = paste(Year, sMonth, sep="_")
vYear_Month = unique(Y$Year_Month)
vMatch = match(vYear_Month, Z$Year_Month)
Z = Z[vMatch,]

  for(i in svVar)
  {
Z[, i] = aggregate(Y[,i], list(Y$Year_Month), sum, na.rm=T)$x    
  }

svTime = c("Past", "Pres", "Fut")

  for(i in svTime)
  {
sIndex = paste("MTI", i, sep="_") 
vPos = Z[,paste(i, "pos", sep="_")]
vNeg = Z[,paste(i, "neg", sep="_")]
vNeu = Z[,paste(i, "neu", sep="_")]
vTot = vPos + vNeg + vNeu

Z[,sIndex] = 100 * (vPos - vNeg) / vTot 
  }

  for(i in c("All", "Pres", "Fut"))
  {
Z[, paste(i, "volat", sep="_")] = aggregate(Y[,paste("Index", i, sep="_")], list(Y$Year_Month), sd, na.rm=T)$x    
  }

Z$MTI_Klima = sqrt((Z$MTI_Pres + 100) * (Z$MTI_Fut + 100))

#-------------------------------------------------------------------------------
#--- Saving output

NObs = nrow(Z)
sBegDate = paste(Z$Year[1], sMonth[1], sep="m")
sEndDate = paste(Z$Year[NObs], sMonth[NObs], sep="m")
sOutFile2 = paste("MediaTenor_Economic_", sBegDate, "_", sEndDate, ".csv", sep="")

write.csv(Z, paste(sFolder, sOutFile2, sep=""), na="", row.names=F)

#-------------------------------------------------------------------------------
#--- Plot graphs

vSel_t = seq(1, NObs, 12)

plot(Z$MTI_Klima, type="l", lwd=3, col="cyan4", xaxt="n", xlab="", ylab="")
axis(1, at=vSel_t, labels=Z$Year[vSel_t])

  for(i in vSel_t)
  {
abline(v=i, lty=2, col="gray70")
  }


  for(i in seq(40, 120, 20))
  {
abline(h=i, lty=2, col="gray70")
  }
#-------------------------------------------------------------------------------

proc.time() - Begin
