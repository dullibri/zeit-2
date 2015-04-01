DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data'
DirCode='h:/Git/zeit-2/data'
# DirCode='f:'



firstyear=1948
lastyear=2015
nyear=lastyear-firstyear+1

# creating data.frame for pure data
oecd=data.frame(m=rep(1:12,nyear)
              ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
              ,year=rep(firstyear:lastyear,each=12))
oecd$ym=paste(oecd$year,oecd$month,sep='-')

# creating data.frame for metadata

oecdmeta=data.frame('name'=character(0)
                    ,'id'=numeric(0)
                    ,'L'=numeric(0)
                    ,'D'=numeric(0)
                    ,'dln'=numeric(0)
                    ,'d2ln'=numeric(0)
                    ,'lag'=numeric(0)
                    ,'code'=character(0)
                    ,stringsAsFactors=F
                    )
# ifo
df=read.csv(paste(DirCode,'/oecd/mei_cli.csv'
                  ,sep='')
            ,sep=','
            ,stringsAsFactors=F
            ,skip=0
#             ,row.names=1
)
variables=unique(df[,'SUBJECT'])
df1=df[df[,1]==variables[1],]
df2=df[df[,1]==variables[2],]
df3=df[df[,1]==variables[3],]
oecd[oecd$ym%in%df1[,'TIME'],variables[1]]=df1[,'Value']
oecd[oecd$ym%in%df2[,'TIME'],variables[2]]=df2[,'Value']
oecd[oecd$ym%in%df3[,'TIME'],variables[3]]=df3[,'Value']
colnames(oecd)[(ncol(oecd)-2):ncol(oecd)]=c('OECDL1','OECDL3','OECDL2')
oecdmeta[1:3,'lag']=2
oecdmeta[,'code']=c(df1$SUBJECT[1],df2$SUBJECT[1],df3$SUBJECT[1])

oecdmeta[,'name']=c('Composite leading indicator (amplitude restored)',
        'Composite leading indicator (normalized)',
        'Composite leading indicator (trend restored)')
oecdmeta[which(oecdmeta$name%in%c('Composite leading indicator (amplitude restored)',
           'Composite leading indicator (normalized)')),c('L')]=1
oecdmeta[which(oecdmeta$name%in%c('Composite leading indicator (amplitude restored)',
           'Composite leading indicator (normalized)',
           'Composite leading indicator (trend restored)')),c('D')]=1
row.names(oecdmeta)=c('OECDL1','OECDL3','OECDL2')
oecdmeta$Source='OECD'
oecdmeta$id=c(93,95,94)




write.csv(oecd,paste(DirCode,'/oecd/oecd.csv',sep=''))
write.csv(oecdmeta,paste(DirCode,'/oecd/oecdmeta.csv',sep=''))
