DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data/buba/data'
DirCode='h:/Git/zeit-2/data/buba/data'
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

# creating data.frame for pure data
buba=data.frame(m=rep(1:12,nyear)
              ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
              ,year=rep(firstyear:lastyear,each=12))
buba$ym=paste(buba$year,buba$month,sep='-')

# creating data.frame for metadata

bubameta=data.frame('name'=character(0)
                    ,'id'=numeric(0)
                    ,'L'=numeric(0)
                    ,'D'=numeric(0)
                    ,'D.ln'=numeric(0)
                    ,'D2ln'=numeric(0)
                    ,'lag'=numeric(0)
                    ,'code'=character(0)
                    ,stringsAsFactors=F
                    )

# money market 
tt=grep('BBK01.SU0101|BBK01.SU0304',df$Code)
tt=df[tt,]
bubameta['IS-M',c('L','D')]=1
bubameta['IS-M',c('code')]=paste(tt$Code,collapse=',')
bubameta['IS-M','name']=c('Money market rate (mth.avg.)')
bubameta['IS-M','lag']=0
bubameta['IS-M','id']=1
bubameta['IS-M','Source']='Buba'

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

bubameta['IS-D',c('L','D')]=1
bubameta['IS-D',c('code')]=paste(tt$Code[c(1,3,4)],collapse=',')
bubameta['IS-D','name']=c('Discount rate/short term repo rate (mth.avg.)')
bubameta['IS-D','lag']=0
bubameta['IS-D','id']=2
bubameta['IS-D','Source']='Buba'
rm('file1','file2','file3','file4','t1','t2','t3','t4')


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


bubameta['IS-3M',c('L','D')]=1
bubameta['IS-3M',c('code')]=paste(tt$Code[c(1,2,3)],collapse=',')
bubameta['IS-3M','name']=c('3m-money market rate (mth.avg.)')
bubameta['IS-3M','lag']=0
bubameta['IS-3M','id']=3
bubameta['IS-3M','Source']='Buba'

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


bubameta['IL-3',c('L','D')]=1
bubameta['IL-3',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['IL-3','name']=c('Yields on debt securities outstanding (mat.3-5 years)')
bubameta['IL-3','lag']=0
bubameta['IL-3','id']=4
bubameta['IL-3','Source']='Buba'

file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='IL-5'
buba[buba$ym%in%t1$X,'IL-5']=t1[,2]

bubameta['IL-5',c('L','D')]=1
bubameta['IL-5',c('code')]=paste(tt$Code[c(2)],collapse=',')
bubameta['IL-5','name']=c('Yields on debt securities outstanding (mat.5-8 years)')
bubameta['IL-5','lag']=0
bubameta['IL-5','id']=5
bubameta['IL-5','Source']='Buba'

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

bubameta['IL-10',c('L','D')]=1
bubameta['IL-10',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['IL-10','name']=c('Long term government bond yield-9-10 years')
bubameta['IL-10','lag']=0
bubameta['IL-10','id']=6
bubameta['IL-10','Source']='Buba'

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
bubameta['SPR-C-G',c('L')]=1
bubameta['SPR-C-G',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['SPR-C-G','name']=c('Corporate bond-government bonds')
bubameta['SPR-C-G','lag']=0
bubameta['SPR-C-G','id']=11
bubameta['SPR-C-G','Source']='Buba'

# BBQFS.M.W0.CORP.RSK_HY_EUR._X.0000 Risikoaufschläge von Euro-Unternehmensanleihen im Non-Investment-Grade-Segment (FSB 2014)

# spreads
buba[,'SPR-10Y-M']=buba[,'IL-10']-buba[,'IS-M']
buba[,'SPR-10Y-D']=buba[,'IL-10']-buba[,'IS-D']
buba[,'SPR-10Y-3M']=buba[,'IL-10']-buba[,'IS-3M']
buba[,'SPR-1D-M']=buba[,'IS-D']-buba[,'IS-M']


termsprnames=c('SPR-10Y-M','SPR-10Y-D','SPR-10Y-3M','SPR-1D-M')
bubameta[termsprnames,c('L')]=1
bubameta[termsprnames,'name']=c('Term spread (10y - money market rate)'
                                ,'Term spread (10y - discount rate)'
                                ,'Term spread (10y - 3 month-money market rate)'
                                ,'Term spread (discount rate - money market rate)'
                                )
bubameta[termsprnames,'lag']=0
bubameta[termsprnames,'code']='see constituent series'
bubameta[termsprnames,'id']=7:10
bubameta[termsprnames,'Source']='Buba'

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

bubameta['EX',c('D.ln')]=1
bubameta['EX',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['EX','name']=c('Nominal effective exchange rate')
bubameta['EX','lag']=1
bubameta['EX','id']=18
bubameta['EX','Source']='Buba'

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
bubameta['EXR','Source']='Buba'

bubameta['EXR',c('D.ln')]=1
bubameta['EXR',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['EXR','name']=c('Real effective exchange rate')
bubameta['EXR','lag']=1
bubameta['EXR','id']=19

# DAX
file1=paste(DirCode,'/BuBa_2015_03/','BBQFS.M.DE.CORP.PRICE_DAX._X.0000','.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='DAX'
buba[buba$ym%in%t1$X,'DAX']=t1[,2]

bubameta['DAX',c('D.ln')]=1
bubameta['DAX',c('code')]='BBQFS.M.DE.CORP.PRICE_DAX._X.0000'
bubameta['DAX','name']=c('DAX')
bubameta['DAX','lag']=0
bubameta['DAX','id']=20

file1=paste(DirCode,'/BuBa_2015_03/','BBQFS.M.DE.0000.VOL_IMP_DAX._X.0000','.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='VOLA1'
buba[buba$ym%in%t1$X,'VOLA1']=t1[,2]

bubameta['VOLA1',c('L','D')]=1
bubameta['VOLA1',c('code')]='BBQFS.M.DE.0000.VOL_IMP_DAX._X.0000'
bubameta['VOLA1','name']=c('DAX vola new')
bubameta['VOLA1','lag']=0
bubameta['VOLA1','id']=21

file1=paste(DirCode,'/BuBa_2015_03/','BBQFS.M.DE.0000.VOL_HIS_DAX._X.0000','.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='VOLA2'
buba[buba$ym%in%t1$X,'VOLA2']=t1[,2]

bubameta['VOLA2',c('L','D')]=1
bubameta['VOLA2',c('code')]='BBQFS.M.DE.0000.VOL_HIS_DAX._X.0000 '
bubameta['VOLA2','name']=c('DAX vola old')
bubameta['VOLA2','lag']=0
bubameta['VOLA2','id']=22

bubameta[c('DAX','VOLA1','VOLA2'),'Source']='Buba'
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

bubameta['HWWA-E',c('D.ln','D2ln')]=1
bubameta['HWWA-E',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['HWWA-E','name']=c('Hwwa index ~,energy')
bubameta['HWWA-E','lag']=1
bubameta['HWWA-E','id']=31


file1=paste(DirCode,'/BuBa_2015_03/',tt$Code[2],'.csv',sep='')
t1=read.csv(file1,stringsAsFactors=F)
t1=t1[which(t1$X%in%buba$ym),1:2]
t1[,2]=as.numeric(t1[,2])
colnames(t1)[2]='HWWA-EX'
buba[buba$ym%in%t1$X,'HWWA-EX']=t1[,2]
bubameta['HWWA-EX',c('D.ln','D2ln')]=1
bubameta['HWWA-EX',c('code')]=paste(tt$Code[c(1)],collapse=',')
bubameta['HWWA-EX','name']=c('Hwwa index ~,excl. energy')
bubameta['HWWA-EX','lag']=1
bubameta['HWWA-EX','id']=33
bubameta[c('HWWA-E','HWWA-EX'),'Source']='Buba'
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
write.csv(bubameta,paste(DirCode,'/bubameta.csv',sep=''))
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