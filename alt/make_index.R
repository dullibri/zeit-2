
# Description ------------------------------------------------------------

# loads "Ergebnis.csv"-Files containing the results of sentiment analysis


# Letzte Modifikation -------------------------------------------------------------------
# 2014-12-08, 


# Load the necessary libraries and defining functions

library(zoo)
# Setting directories for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit" # text files are stored here
DirCode='H:/git/zeit-2' # main directory
setwd(DirCode)

# Load register created by 'Getting_register.R' ---------------------------
load(paste(DirCode,"/register.RData",sep=''))

# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)


# Marking economics sections' articles ------------------------------------------------------------------------

# load(paste(DirCode,"/e_register.RData",sep=''))
# register$eco='n'
# neco=nrow(e_register)
# for (i in 1:neco){
#         register[which(register$link==e_register$link[i]),'eco']='y'
# }
# 
# eco=sapply(register$link,function(x) x%in%e_register$link)


# Getting "Ergebnis.csv" Files and integrate them into register -----------


# register$id=NA
# register$npword=NA
# register$nnword=NA
# register$nword=NA
# register$pvalue=NA
# register$nvalue=NA
# for (subd in listsubdirs){
#         if (subd=='1993.36'){next}
#         Ergebnis=read.csv(paste(DirRawTexts,'/',subd,'/','Ergebnis.csv',sep=''),row.names=1)
#         narticle=nrow(Ergebnis)
#         for (article in 1:narticle){
#                 register[Ergebnis$id[article],5:10]=Ergebnis[article,]
#         }
#         
# }
# rm(Ergenbis)

# Adding dates ------------------------------------------------------------

# BegYear = 1990
# BegMonth = 1
# BegDay = 1
# EndYear = 2014
# EndMonth = 12
# EndDay = 31
# 
# sBegDate = paste(BegDay, BegMonth, BegYear, sep=".")
# sEndDate = paste(EndDay, EndMonth, EndYear, sep=".")
# Date = seq(as.Date(sBegDate, format="%d.%m.%Y"), as.Date(sEndDate, format="%d.%m.%Y"), by="days")
# Tdays=Date[which(weekdays(Date)=='Thursday')]
# 
# dates=data.frame(NA)
# 
# dates=data.frame(year=format(Tdays,'%Y')
#                  ,month=format(Tdays,'%m')
#                  ,day=format(Tdays,'%d')
# )
# table(dates$year)
# 
# 
# dates$issue=NA
# years=unique(dates$year)
# nyears=length(years)
# for (year in years){
#         nyissue=nrow(dates[which(year==dates$year),])
#         dates[which(year==dates$year),'issue']=1:nyissue
#         
# }
# rm(year)
# dates$year.issue=NA
# register$day=NA
# register$month=NA
# for (i in 1:nrow(dates)){
#         ids=which(register$year==dates$year[i]&register$issue==dates$issue[i])
#         register[ids,'day']=dates$day[i]
#         register[ids,'month']=dates$month[i]
# }
# rm(dates,listsubdirs,Date,EndYear,i,BegDay,BegMonth,BegYear,EndDay,EndMonth,Tdays,ids,nyears,nyissue,sBegDate,sEndDate,years)


# Getting metadata out of the html files ----------------------------------

# register$title_in_text=NA
# register$date=NA
# register$keywords=NA
# 
# for (i in 1:nrow(register)){
#         plainhtml <- read.csv(paste(DirRawTexts,'/',register$year[i],'.',register$issue[i],'/',i,'.txt',sep=''))[-1]
#         # plainhtml <-c(plainhtml)
#         plainhtml=apply(plainhtml,2,as.character)
#         plainhtml<-paste(plainhtml,sep="",collapse="")
#         
#         title_index=regexec(paste('<title>','(.*)',' DIE ZEIT Archiv',sep=''),plainhtml)
#         register$title_in_text[i]=regmatches(plainhtml,title_index)[[1]][2]
#         
#         date_index=regexec(paste('date" content="','([0-9]{4}-[0-9]{2}-[0-9]{2})',sep=''),plainhtml)
#         register$date[i]=regmatches(plainhtml,date_index)[[1]][2]
#         
#         keywords_index=regexec(paste('keywords" content="','(.*)','\"><(meta property)=\"og:site_name\"',sep=''),plainhtml)
#         register$keywords[i]=regmatches(plainhtml,keywords_index)[[1]][2]
# }


# calculating relative values ---------------------------------------------
register$perc_pword=register$npword/register$nword
register$perc_nword=register$nnword/register$nword
register$perc_pnword=(register$npword+register$nnword)/register$nword
register$rpvalue=register$pvalue/register$npword
register$rpvalue[register$npword==0]=0
register$rnvalue=register$nvalue/register$nnword
register$rnvalue[register$nnword==0]=0
register$rvalue=(register$pvalue+register$nvalue)/(register$npword+register$nnword)
register$yearissue=paste(register$year,register$issue,sep='.')
register=register[-which(register$link=='http://www.zeit.de/1993/36/ein-ganz-legaler-nepp'),]

# aggregating to issues ---------------------------------------------------

Index=aggregate(register[,c("year","month","day")],list(register$yearissue),mean,na.rm=T)
paste0=function(x){if (nchar(x)==1){y=paste('0',x,sep='')
                                    return(y)}else{return(x)}}
Index$Month=sapply(Index$month,paste0)
Index$Day=sapply(Index$day,paste0)
Index$YearMonthDay=as.Date(paste(Index$year,Index$Month,Index$Day,sep='/'),'%Y/%m/%d')

val_gr=c("perc_pnword","perc_nword","perc_pword","npword","nnword","nword","pvalue","nvalue","rpvalue","rnvalue","rvalue")
Index=cbind(Index,aggregate(register[,val_gr],list(register$yearissue),mean,na.rm=T)[,-1])


# aggregating over month --------------------------------------------------

Index$yearmonth=paste(Index$year,Index$Month,sep='/')
Index_m=aggregate(Index,list(Index$yearmonth),mean)
Index_m$yearmonth=NULL
Index_m[,c(2)]=NULL
Index_m[,c('Month','Day','day')]=NULL
Index_m$Month=NULL
Index_m$Day=NULL 
Index_m$day=NULL


test=zoo(Index,Index$YearMonthDay)
# calculating monthly means & stdv. -----------------------------------------------
register$yearmonth=paste(register$year,register$month,sep='.')



register$Month=sapply(register$Month,paste0)
register$Day=sapply(register$day,paste0)
register$YearMonthDay=paste(register$year,register$Month,register$Day,sep='/')
# 
# register$yearmonth=format(register$date,'%Y.%m')

register$YearMonthDay=as.Date(register$YearMonthDay,'%Y/%m/%d')

test=zoo(register,register$YearMonthDay)
# zeitmeans=aggregate(data.frame(positive=register$rpvalue,negative=register$rnvalue,consolidated=register$rvalue),list(register$date),mean)
# zeitmeans_zoo=zoo(zeitmeans[,2:4],zeitmeans$Group.1)
# 
# zeitstdv=aggregate(data.frame(positive=register$rpvalue,negative=register$rnvalue,consolidated=register$rvalue),list(register$date),sd)
# zeitstdv_zoo=zoo(zeitmeans[,2:4],zeitmeans$Group.1)
# plot(rollmean(zeitmeans,k=3))


zeitmeans=aggregate(data.frame(positive=register$rpvalue,negative=register$rnvalue,consolidated=register$rvalue),list(register$yearmonth),mean,na.rm=T)
zeitmeans$Group.1=as.Date(zeitmeans$Group.1,format('%Y.%m'))
zeitmeans_zoo=zoo(zeitmeans[,2:4],zeitmeans$Group.1)

zeitstdv=aggregate(data.frame(positive=register$rpvalue,negative=register$rnvalue,consolidated=register$rvalue),list(register$date),sd)
zeitstdv_zoo=zoo(zeitmeans[,2:4],zeitmeans$Group.1)

plot(rollmean(zeitmeans,k=3))
#  ------------------------------------------------------------------------
# calculating rolling means -----------------------------------------------
#  ------------------------------------------------------------------------


date=format
register$date=paste(register$day,register$month,register$year,sep='.')
register$date = as.Date(register$dat, format="%d.%m.%Y")

# first observation that is based on enough obs  --------------------------

first_obs=register$date[1]
first_mean_obs=seq(first_obs, length = 2, by = "1 months")[2]
nmean_obs=sum(register$date>=first_mean_obs) # number of obs of mean
mdates=unique(register$date[register$date>=first_mean_obs])
rmean=matrix(NA,nrow=length(mdates),ncol=1)
row.names(rmean)=as.character(mdates)

for (idmdate in 1:length(mdates)){
        lower_date=seq(mdates[idmdate], length = 2, by = "-1 months")[2]
        id_rolling_window=which(mdates[idmdate]>=register$date&register$date>lower_date)
        rmean[idmdate]=mean(register$rvalue[id_rolling_window],rm.na=T)
}

plot(rmean)
test=zoo(rmean,mdates)
prices2returns(aggregate(m, as.yearmon, tail, 1))
lastmonth=aggregate(test, as.yearmon, tail, 1)
firstmonth=aggregate(test, as.yearmon, head, 1)
plot(lastmonth)
lines(firstmonth,col='red')
