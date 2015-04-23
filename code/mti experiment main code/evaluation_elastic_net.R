DirCode='h:/Git/zeit-2'
target='IP'
h=1 # horizon
load(paste(DirCode,'/data/realtime_sets_cutoffday_31.RData',sep=''))
vint.n=gsub('-31','',names(sets))
raw=list()
for (i in 1:12){
        file=paste(DirCode,'/results/elanet_ranking',target,'_h',i,'.csv',sep='')
        raw[[i]]<- read.csv(file, header=T,stringsAsFactors=F,row.names=1)
}
sel=35
for (i in 1:12){
        
        res.aux=data.frame(av.rk=rowSums(raw[[i]])/ncol(raw[[i]])
                           ,nsel=rowSums(raw[[i]]<=sel)/ncol(raw[[i]])
                           )
        res.aux$most.rk=rank(res.aux$nsel,ties.method='max')
        res.aux$most.rk=max(res.aux$most.rk)+1-res.aux$most.rk
        
        if (i==1){
                
                colnames(res.aux)=paste(colnames(res.aux),i,sep='_')
                result=res.aux
        }
        if (i!=1){
                colnames(res.aux)=paste(colnames(res.aux),i,sep='_')
                result=cbind(result,res.aux)
        }
}


mt.ind=grep('MT.',row.names(result))
res.mt=result[mt.ind,]
t=res.mt[,grep('most.rk',colnames(res.mt))]
t=res.mt[,grep('nsel',colnames(res.mt))]

# mt.ind=grep('zeit',row.names(result))
# res.mt=result[mt.ind,]

# special periods

t=raw[[11]]
colnames(t)=vint.n
t=t[grep('MT.',row.names(t)),]
# t=t[grep('Zeit',row.names(t)),]
write.csv(res.mt,'tt.csv')
