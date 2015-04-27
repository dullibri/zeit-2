# This file collects and merges the data sets and metadata
dir='H:/git/zeit-2/Data'
metaf=list.files(paste(dir,'/metadata',sep=''))

t1=read.csv(paste(dir,'/metadata/',metaf[1],sep=''),row.names=1)
t2=read.csv(paste(dir,'/metadata/',metaf[2],sep=''),row.names=1)
t3=read.csv(paste(dir,'/metadata/',metaf[3],sep=''),row.names=1)
t4=read.csv(paste(dir,'/metadata/',metaf[4],sep=''),row.names=1)
t5=read.csv(paste(dir,'/metadata/',metaf[5],sep=''),row.names=1)
t6=read.csv(paste(dir,'/metadata/',metaf[6],sep=''),row.names=1)
t7=read.csv(paste(dir,'/metadata/',metaf[7],sep=''),row.names=1)
t8=read.csv(paste(dir,'/metadata/',metaf[8],sep=''),row.names=1)
t9=read.csv(paste(dir,'/metadata/',metaf[9],sep=''),row.names=1)

meta=rbind(t4,t3,t2,t1,t5,t6,t7,t8,t9)
meta$deflated=0
meta$used=1

deflated=grep("deflatedvarmeta.csv",metaf)
eval(parse(text=paste('defl=t',deflated,sep='')))
defl=row.names(defl)
meta[c('HWWA','HWWA-E','HWWA-EX','OIL'),'deflated']=1

missing=grep("missingvariablesmeta.csv",metaf)
eval(parse(text=paste('miss=t',missing,sep='')))
miss=row.names(miss)
meta[miss,'used']=0

meta=meta[sort(meta$id,index.return=T)$ix,]
write.csv(meta,paste(dir,'/metadata.csv',sep=''))
