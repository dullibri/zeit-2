DirCode='H:/git/zeit-2' # main directory





meta=read.csv(paste(DirCode,'/data/zeit indikatoren/article_ergebnis.csv',sep='')
              ,stringsAsFactors=F
)
meta$issue=paste(meta$i_year,'.',meta$i_month,sep='')

meta=meta[,-1]
colnames(meta)[grep('art_num',colnames(meta))]='id'
meta$id=paste(meta$issue,'.',meta$id,sep='')


# correcting some dates  --------------------------------------------------

most.freq.date=function(bymeta){
        datesissue=bymeta$date
        totaldatesissue=length(datesissue)
        
        if ((sum(datesissue=='1998-04-23')+sum(datesissue=='1899-12-31'))==totaldatesissue){
                mfreqdate='1998-04-23'
                return(mfreqdate)
        }
        if (sum(datesissue=='1899-12-31')>0){
                datesissue=datesissue[-which(datesissue=='1899-12-31')]}
        if (sum(datesissue=='1998-04-23')>0){
                datesissue=datesissue[-which(datesissue=='1998-04-23')]}
        
        ndates=length(datesissue)
        dates=table(datesissue)
        mfreq=which.max(dates)
        mfreqdate=names(dates)[mfreq] 
        
        return(mfreqdate)
}
newdate=by(meta[,'date',drop=F],INDICES=meta$issue,FUN=most.freq.date)
narticle=nrow(meta)
t=sapply(1:narticle,function(x) newdate[as.character(meta$issue[x])] )
t=unlist(t)
# meta$olddate=meta$date
meta$date=t

# individual corrections
meta[which(meta$issue=='2008.40'),'date']='2008-09-25'
meta[which(meta$issue=='2008.44'),'date']='2008-10-23'

plot(table(meta$date))

# correcting year, month and day
t=strsplit(meta$date,'-')#[[1]][1]
meta$year=sapply(t,function(x) x[1])
meta$month=sapply(t,function(x) x[2])
meta$day=sapply(t,function(x) x[3])
meta.s=meta
meta$ym=paste(meta$year,meta$month,sep='.')
plot(table(meta$ym))


# getting evaluations -----------------------------------------------------


bew=read.csv(paste(DirCode,'/data/zeit indikatoren/zeitbewertungen.csv',sep='')
             ,stringsAsFactors=F
)
bew=bew[,-1]
bew$id=paste(bew$year,bew$issue,bew$id,sep='.')

err=which(!meta$id%in%bew$id)
t=meta[err,]
meta=meta[-err,]


# sort.ind=sort(meta$id,index.return=T)[[2]]
# meta=meta[sort.ind,]

meta$id=as.character(meta$id)
meta$issue=as.character(meta$issue)

bew$id=as.character(bew$id)
bew$issue=as.character(bew$issue)

total <- merge(meta,bew,'id')

sents=grep('sent',colnames(total))
total[total==-999]=0

# getting indicator monthly averages --------------------------------------
ind.n=c("qdap_value","qdap_nword","sent_nneg","sent_npos","sent_negv","sent_posv","sent_nwords","sent_val","posextind","negextind","valuext")
t=aggregate(total[,ind.n],list(as.factor(total$ym)),mean)
t.median=aggregate(total[,ind.n],list(as.factor(total$ym)),median)
t=cbind(t,t.median)
write.csv(t,paste(DirCode,'/data/zeit indikatoren/zeit.indexes.csv',sep=''))

# plot(t$qdap_value
#      ,type='l'
#      ,ylab=''
#      ,xlab=''
#      ,
#      )
