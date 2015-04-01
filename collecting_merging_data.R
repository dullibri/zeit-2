# This file collects and merges the data sets located in "data/finaldata"
dir='H:/git/zeit-2/Data'
dataf=list.files(paste(dir,'/finaldata',sep=''))
dataf=dataf[-grep('\\.R',dataf)]
dataf=dataf[-grep('EC',dataf)]
t1=read.csv(paste(dir,'/finaldata/',dataf[1],sep=''))
t2=read.csv(paste(dir,'/finaldata/',dataf[2],sep=''))
t3=read.csv(paste(dir,'/finaldata/',dataf[3],sep=''))
t4=read.csv(paste(dir,'/finaldata/',dataf[4],sep=''))
t5=read.csv(paste(dir,'/finaldata/',dataf[5],sep=''))

firstyear=1948
lastyear=2015
nyear=lastyear-firstyear+1

# creating data.frame for pure data
data=data.frame(m=rep(1:12,nyear)
                ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
                ,year=rep(firstyear:lastyear,each=12))
data$ym=paste(data$year,data$month,sep='-')

t=t1[,(grep('ym',colnames(t1))+1):ncol(t1)]
data[,colnames(t1)[(grep('ym',colnames(t1))+1):ncol(t1)]]=NA
data[data$ym%in%t1$ym,colnames(t1)[(grep('ym',colnames(t1))+1):ncol(t1)]]=t1[,(grep('ym',colnames(t1))+1):ncol(t1)]

t=t2[,(grep('ym',colnames(t2))+1):ncol(t2)]
data[,colnames(t2)[(grep('ym',colnames(t2))+1):ncol(t2)]]=NA
data[data$ym%in%t2$ym,colnames(t2)[(grep('ym',colnames(t2))+1):ncol(t2)]]=t2[,(grep('ym',colnames(t2))+1):ncol(t2)]

t=t3[,(grep('ym3',colnames(t3))+1):ncol(t3)]
data[,colnames(t3)[(grep('ym3',colnames(t3))+1):ncol(t3)]]=NA
data[data$ym%in%t3$ym,colnames(t3)[(grep('ym3',colnames(t3))+1):ncol(t3)]]=t3[,(grep('ym3',colnames(t3))+1):ncol(t3)]

t=t4[,(grep('ym',colnames(t4))+1):ncol(t4)]
data[,colnames(t4)[(grep('ym',colnames(t4))+1):ncol(t4)]]=NA
data[data$ym%in%t4$ym,colnames(t4)[(grep('ym',colnames(t4))+1):ncol(t4)]]=t4[,(grep('ym',colnames(t4))+1):ncol(t4)]

t=t5[,(grep('ym',colnames(t5))+1):ncol(t5)]
data[,colnames(t5)[(grep('ym',colnames(t5))+1):ncol(t5)]]=NA
data[data$ym%in%t5$ym,colnames(t5)[(grep('ym',colnames(t5))+1):ncol(t5)]]=t5[,(grep('ym',colnames(t5))+1):ncol(t5)]

# check for completeness --------------------------------------------------

metadata <- read.csv("H:/git/zeit-2/Data/metadata.csv", header=FALSE)
t=colnames(data)
t=gsub('\\.','-',t)
metadata$V1[!metadata$V1%in%t]
notin=metadata[!metadata$V1%in%t,]
notin=notin[-1,] # header

notinrev=t[!t%in%metadata$V1]

write.csv(data,paste(dir,'/data.csv',sep=''))
