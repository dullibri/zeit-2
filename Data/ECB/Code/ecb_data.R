DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data/ecb/data'
# DirCode='h:/Git/zeit-2/data/buba/data'
# DirCode='f:'


m3=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

firstyear=1948
lastyear=2015
nyear=lastyear-firstyear+1

# creating data.frame for pure data
ecb=data.frame(m=rep(1:12,nyear)
              ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
              ,m3=rep(m3,nyear)
              ,year=rep(firstyear:lastyear,each=12))
ecb$ym=paste(ecb$year,ecb$month,sep='-')
ecb$ym3=paste(ecb$year,ecb$m3,sep='')
# creating data.frame for metadata

ecbmeta=data.frame('name'=character(0)
                    ,'id'=numeric(0)
                    ,'level'=numeric(0)
                    ,'d'=numeric(0)
                    ,'dln'=numeric(0)
                    ,'d2ln'=numeric(0)
                    ,'lag'=numeric(0)
                    ,'code'=character(0)
                    ,stringsAsFactors=F
                    )


# oil 


file1=paste(DirCode,'/oil price.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
code=t1[1,2]
t1=t1[which(t1[,1]%in%ecb$ym3),1:2]
ecb[which(ecb$ym3%in%t1[,1]),'OIL']=t1[,2]

ecbmeta['OIL',c('dln','d2ln')]=1
ecbmeta['OIL',c('code')]=code
ecbmeta['OIL','name']=c('Money market rate (mth.avg.)')
ecbmeta['OIL','lag']=0
ecbmeta['OIL','id']=1


write.csv(ecb,paste(DirCode,'/ecb.csv',sep=''))
write.csv(ecbmeta,paste(DirCode,'/ecbmeta.csv',sep=''))
