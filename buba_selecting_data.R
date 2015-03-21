DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data/buba/data'
# DirCode='h:/Git/zeit-2/data/buba/data'
# DirCode='f:'
df=read.csv(paste(DirCode,'/Buba_Detailed_Variables_List_EN.csv'
                            ,sep='')
                      ,sep=','
                      ,stringsAsFactors=F
#                       ,row.names=1
)
t=df[,2]
firstyear=1948
lastyear=2015
nyear=lastyear-firstyear+1
buba=data.frame(m=rep(1:12,nyear)
              ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
              ,year=rep(firstyear:lastyear,each=12))
buba$ym=paste(buba$year,buba$month,sep='-')

# money market 
tt=grep('BBK01.SU0101|BBK01.SU0304',df$Code)
tt=df[tt,]

file2=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
t2=read.csv(file2,stringsAsFactors=F)
colnames(t2)[2]='IS-M'
t2=t2[grep('1959-12',t2$X):grep('1998-12',t2$X),1:2]
t2[,2]=as.numeric(t2[,2])
buba[which(buba$ym%in%t2[,1]),'IS-M']=t2[,2]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[grep('1999-01',t1$X):grep('2015-01',t1$X),1:2]
colnames(t1)[2]='IS-M'
t1[,2]=as.numeric(t1[,2])
buba[which(buba$ym%in%t1[,1]),'IS-M']=t1[,2]



# discount rate
tt=grep('BBK01.SU011',df$Code)
tt=df[tt,]


file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
colnames(t1)[2]='IS-D'
t1=t1[grep('1948-07',t1$X):grep('1998-12',t1$X),1:2]
t1[,2]=as.numeric(t1[,2])
buba[which(buba$ym%in%t1[,1]),'IS-D']=t1[,2]

file3=paste(DirCode,'/BuBa_2015_03/',tt$Code[3],'.csv',sep='')
t3=read.csv(file3,stringsAsFactors=F)
colnames(t3)[2]='IS-D'
t3=t3[grep('1999-01',t3$X):grep('2001-12',t3$X),1:2]
t3[,2]=as.numeric(t3[,2])
buba[which(buba$ym%in%t3[,1]),'IS-D']=t3[,2]

file4=paste(DirCode,'/BuBa_2015_03/',tt$Code[4],'.csv',sep='')
t4=read.csv(file4,stringsAsFactors=F)
colnames(t4)[2]='IS-D'
t4=t4[grep('2002-01',t4$X):grep('2015-01',t4$X),1:2]
t4[,2]=as.numeric(t4[,2])
buba[which(buba$ym%in%t4[,1]),'IS-D']=t4[,2]

rm('file1','file2','file3','file4','t1','t2','t3','t4')


to=paste(DirCode,'selection',vari,sep='/')
variables=rbind(variables,tt)


# money market 3month 
tt=grep('[mM]oney market rate',t)
m3rate=c('BBK01.SU0107','BBK01.SU0268','BBK01.SU0316')
tt=df[which(df$Code%in%m3rate),]

# frankfurt bank rate is the longest. where replaced by fibor or euribor will
# be subsequently be overwritten.
file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IS-3M'
buba[buba$ym%in%t1$X,'IS-3M']=t1[,2]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IS-3M'
buba[buba$ym%in%t1$X,'IS-3M']=t1[,2]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[3],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IS-3M'
buba[buba$ym%in%t1$X,'IS-3M']=t1[,2]

# yields on debt security outstanding 3-5 years and 5-8
tt=grep('Yields on debt securities outstanding',t)
tt=df[tt,]
tt=tt[grep('[mM]onthly',tt[,2]),]
tt=tt[grep('3|4|5|6|7|8|9|10',tt[,2]),]
tt=tt[grep('Listed Federal securities',tt[,2]),]
tt=tt[c(1,2),]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IL-3'
buba[buba$ym%in%t1$X,'IL-3']=t1[,2]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IL-5'
buba[buba$ym%in%t1$X,'IL-5']=t1[,2]

# yields on debt security outstanding 9-10 years
tt=grep('Yields',t)
tt=df[tt,]
tt=tt[grep('9|10',tt[,2]),]
tt=tt[17,]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IL-10'
buba[buba$ym%in%t1$X,'IL-10']=t1[,2]

# corporate 
tt=df[grep('BBK01.WU0022',df$Code),]
file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='C'
buba[buba$ym%in%t1$X,'C']=t1[,2]
buba[,'SPR-C-G']=buba[,'C']-buba[,'IL-3']
buba=buba[,-grep('C',colnames(buba))[1]]
# BBQFS.M.W0.CORP.RSK_HY_EUR._X.0000 Risikoaufschläge von Euro-Unternehmensanleihen im Non-Investment-Grade-Segment (FSB 2014)

# spreads
buba[,'SPR-10Y-M']=buba[,'IL-10']-buba[,'IS-M']
buba[,'SPR-10Y-D']=buba[,'IL-10']-buba[,'IS-D']
buba[,'SPR-10Y-3M']=buba[,'IL-10']-buba[,'IS-3M']
buba[,'SPR-1D-M']=buba[,'IS-D']-buba[,'IS-M']
buba[,'SPR-1D-M']=buba[,'IS-D']-buba[,'IS-M']


# nominal effective exchange rate
tt=grep('[nN]ominal effective exchange rate',t)
tt=df[tt,]
tt=tt[grep('EER-39',tt[,2]),]
tt=tt[3,]
file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='EX'
buba[buba$ym%in%t1$X,'EX']=t1[,2]


# real effective exchange rate
tt=grep('[Rr]eal effective exchange rate',t)
tt=df[tt,]
tt=tt[grep('EER-39',tt[,2]),]
tt=tt[3,]
file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='EXR'
buba[buba$ym%in%t1$X,'EXR']=t1[,2]

# m1 

# Details on the bundesbank page about the series reveal, that m1-m3 are not useful:
# Allgemein:         Die deutschen Beiträge zu den monetären Aggregaten des Eurosystems sind keinesfalls als eigene nationale Geldmengenaggregate zu interpretieren und damit auch nicht mit den früheren deutschen Geldbeständen M1, M2 oder M3 vergleichbar. M2 zuzüglich Repogeschäfte, Geldmarktfondsanteile und Geldmarktpapiere sowie Schuldverschreibungen bis zu 2 Jahren.
# Methodik: 	Die Angaben sind mit den bis Ende 1998 für Deutschland veröffentlichten Zahlen wegen unterschiedlicher Positionsinhalte und abweichender Berichtskreise nicht vergleichbar. Ab 01.2002 ohne Bargeldumla
# tt=grep('BBK01.TXI301|BBK01.TXI302|BBK01.TXI303',df$Code)
# tt=df[tt,]
# 
# file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
# t1=read.csv(file1,stringsAsFactors=F)
# t1=t1[which(t1$X%in%buba$ym),1:2]
# t1[,2]=as.numeric(t1[,2])
# colnames(t1)[2]='M1'
# buba[buba$ym%in%t1$X,'M1']=t1[,2]
# 
# file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
# t1=read.csv(file1,stringsAsFactors=F)
# t1=t1[which(t1$X%in%buba$ym),1:2]
# t1[,2]=as.numeric(t1[,2])
# colnames(t1)[2]='M2'
# buba[buba$ym%in%t1$X,'M2']=t1[,2]
# 
# file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[3],'.csv',sep='')
# t1=read.csv(file1,stringsAsFactors=F)
# t1=t1[which(t1$X%in%buba$ym),1:2]
# t1[,2]=as.numeric(t1[,2])
# colnames(t1)[2]='M3'
# buba[buba$ym%in%t1$X,'M3']=t1[,2]


# HWWA
tt=grep('HWWI',t)
tt=df[tt,]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[1],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='HWWA-E'
buba[buba$ym%in%t1$X,'HWWA-E']=t1[,2]

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='HWWA-EX'
buba[buba$ym%in%t1$X,'HWWA-EX']=t1[,2]


# # spread 10years discount corporate 
# tt=grep('BBK01.WX0032',df$Code)
# tt=df[tt,]
# tt$label=c('spr-c-g')
# variables=rbind(variables,tt)

# 
# #corporate
# tt=grep('BBK01.WU0022',df$Code)
# tt=df[tt,]
# tt$label=c('spr-c-g')
# variables=rbind(variables,tt)
# 
# 
# #corporate
# tt=grep('BBQFS.M.W0.CORP.RSK_HY_EUR._X.0000',df$Code)
# tt=df[tt,]
# # tt$label=c('spr-c-g')
# variables=rbind(variables,tt)
# 
# 
# 
# 
# 
# # BBQFS.M.W0.CORP.RSK_HY_EUR._X.0000
# 
# for (var in variables$Code){#var=variables$Code[1]
#         var=paste(var,'.csv',sep='')
#         from=paste(DirCode,'BuBa_2015_03',var,sep='/')
#         to=paste(DirCode,'selection',var,sep='/')
#         file.copy(from, to)
# }
# 
# 
# year=rep(1990:2015,each=12)
# month=rep(1:12,2015-1990+1)
# m=c(paste('0',1:9,sep=''),paste(10:12,sep=''))
# m=rep(m,2015-1990+1)
# ym=paste(year,m,sep='-')
# 
# df=data.frame(year=year,month=month)
# row.names(df)=ym
# df[,variables$label]=NA
# write.csv(df,paste(DirCode,'/collection_specimen.csv',sep=''))
write.csv(buba,paste(DirCode,'/buba.csv',sep=''))
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