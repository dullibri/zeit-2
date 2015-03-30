DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data'
DirCode='h:/Git/zeit-2/data'
# DirCode='f:'



firstyear=1948
lastyear=2015
nyear=lastyear-firstyear+1

# creating data.frame for pure data
survey=data.frame(m=rep(1:12,nyear)
              ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
              ,year=rep(firstyear:lastyear,each=12))
survey$ym=paste(survey$year,survey$month,sep='-')

# creating data.frame for metadata

surveymeta=data.frame('name'=character(0)
                    ,'id'=numeric(0)
                    ,'level'=numeric(0)
                    ,'d'=numeric(0)
                    ,'dln'=numeric(0)
                    ,'d2ln'=numeric(0)
                    ,'lag'=numeric(0)
                    ,'code'=character(0)
                    ,stringsAsFactors=F
                    )
# ifo
df=read.csv(paste(DirCode,'/ifo/ifo all.csv'
                  ,sep='')
            ,sep=','
            ,stringsAsFactors=F
            ,skip=1
            ,row.names=1
)
colnames(df)=gsub('\\.','-',colnames(df))

survey[,colnames(df)]=NA
survey[which(survey$ym%in%row.names(df)),colnames(df)]=df[,colnames(df)]

surveymeta[colnames(df),c('level','d')]=1
surveymeta[colnames(df),c('code')]=NA
surveymeta[colnames(df),'name']=c('Ifo index climate','Ifo expectations climate',
                                  'Ifo index manufacturing', 'Ifo expectations manufacturing',
                                  'Ifo index capital goods', 'Ifo expecations capital goods',
                                  'Ifo index intermediate goods','Ifo expectations intermediate goods',
                                  'Ifo index wholesale','Ifo expectations wholesale')
surveymeta[colnames(df),'lag']=0
surveymeta[colnames(df),'id']=c(39:48)




write.csv(survey,paste(DirCode,'/survey.csv',sep=''))
write.csv(surveymeta,paste(DirCode,'/surveymeta.csv',sep=''))
# j=1
# for (var in variables$Code){#var=variables$Code[1]
#         vari=paste(var,'.csv',sep='')
#         to=paste(DirCode,'selection',vari,sep='/')
#         aux=read.csv(to,na.strings='.',skip=4,stringsAsFactors=F)
#         aux.rn=aux[,1]
#         aux.len=vector(mode='character',length=nrow(aux))
#         
#         for (i in 1:nrow(aux)){aux.len[i]=nchar(aux.rn[i])}
#         aux.rn=aux.rn[aux.len>0]
#         df[aux.rn,variables$label[j]]=aux[aux.rn,2]
#         j=j+1
# }