DirCode='H:/git/zeit-2' # main directory
# DirCode='c:/users/dirk/documents/github/zeit-2' # main directory


meta=read.csv(paste(DirCode,'/data/zeit indikatoren/meta_all.csv',sep='')
              ,stringsAsFactors=F
)
# meta0=meta[grep('\\.0',meta$number),]
# meta0$number=gsub('\\.0','\\.',meta0$number)

meta$number=gsub('\\.0','\\.',meta$number)
# meta=read.csv(paste(DirCode,'/data/zeit indikatoren/article_ergebnis.csv',sep='')
#               ,stringsAsFactors=F
# )
# meta$issue=paste(meta$i_year,'.',meta$i_month,sep='')


colnames(meta)[grep('number',colnames(meta))]='id'


meta$issue=gsub('\\.[0-9]{1,8}$','',meta$id)
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



# loading identifications of "inflation" ----------------------------------
infl=read.csv(paste(DirCode,'/data/zeit indikatoren/InflationWord.csv',sep='')
             ,stringsAsFactors=F
)
infl=infl[,-1]
# getting evaluations -----------------------------------------------------
bew=read.csv(paste(DirCode,'/data/zeit indikatoren/Zeitbewertungen.qdap.negator.csv',sep='')
             ,stringsAsFactors=F
)
bew=bew[,-1]
bew$id=paste(bew$year,bew$issue,bew$id,sep='.')
bew[,c('Inflation','InflationMutationen')]=NA
bew[,c('Inflation','InflationMutationen')]=infl[,c('Inflation','InflationMutationen')]


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
total$title=gsub(' \\|','',total$title)
# getting econmocs section only -------------------------------------------

load(paste(DirCode,'/data/zeit indikatoren/e_register.rdata',sep=''))
load(paste(DirCode,'/data/zeit indikatoren/e_register_update.rdata',sep=''))
e_register=rbind(e_register,register)

# matching total und register: link and title ---------------------------------------------

ecoindlink=which(total$link%in%e_register$link)
ecoindtitle=total$title%in%e_register$title
nadel=(is.na(total$title)==T)+(nchar(total$title)==0)
ecoindtitle[nadel==T]=F
ecoindtitle=which(ecoindtitle)

# using "wirtschaft" in keywords  ------------------------------------------------
ecokey=grep('^Wirtschaft',total$keywords)

ecoind=unique(c(ecoindlink,ecoindtitle,ecokey))
# test=total[ecokey,]
# test=table(total[ecoind,'ym'])
# test=total[ecoind[!ecoind%in%ecoindlink],]
# eco1=ecoindlink[!ecoindlink%in%ecoindtitle]
# title_no_link=(ecoind%in%ecoindtitle)&(!ecoind%in%ecoindlink)
# eco2=ecoindtitle[unique(which(ecoindtitle%in%ecoind),which(ecoindlink%in%ecoind))]
# ecoind=ecoind[title_no_link==F]
# # tt=total[eco2,]
# # e_nicht=which(!e_register$link%in%total$link)
# # e_nicht.df=e_register[e_nicht,]
total$eco=F
total$eco[ecoind]=T


# drop articles without rating --------------------------------------------

tt=total==-999
total[tt]=0
# dropping one extreme outlier
total=total[-grep('Der Notenbanker-Rap',total$title),]

# concentrating on the economics section ----------------------------------

total.s=total

total=total[total$eco,]
# write.csv(total,paste(DirCode,'/data/zeit indikatoren/zeit.article.and.eval.csv',sep=''))

# getting indicator monthly averages --------------------------------------

# RELATIVE VALUES PER WORD ------------------------------------------------
# durchschnittliche wortwert

indexrel=aggregate(total[,'qdap_value'],list(as.factor(total$ym)),sum,na.rm=T)
ns=aggregate(total[,'qdap_nword'],list(as.factor(total$ym)),sum,na.rm=T)[,2]
indexrel=data.frame(qdap_negator_rel_val_total=indexrel[,2]/ns)
# Wortwerte je artikel
total[,'qdap_negator_value_rel']=total[,'qdap_value']/total[,'qdap_nword']
indexrel[,paste('qdap_negator_value_rel','_25',sep='')]=aggregate(total[,'qdap_negator_value_rel'],list(as.factor(total$ym)),function(x) quantile(x,probs=c(0.25),na.rm=T))[,2]
indexrel[,paste('qdap_negator_value_rel','_mean',sep='')]=aggregate(total[,'qdap_negator_value_rel'],list(as.factor(total$ym)),mean,na.rm=T)[,2]

indexrel[,paste('qdap_negator_value_rel','_50',sep='')]=aggregate(total[,'qdap_negator_value_rel'],list(as.factor(total$ym)),function(x) quantile(x,probs=c(0.50),na.rm=T))[,2]
indexrel[,paste('qdap_negator_value_rel','_75',sep='')]=aggregate(total[,'qdap_negator_value_rel'],list(as.factor(total$ym)),function(x) quantile(x,probs=c(0.75),na.rm=T))[,2]

indexrel[,paste('qdap_negator_value_rel','_sd',sep='')]=aggregate(total[,'qdap_negator_value_rel'],list(as.factor(total$ym)),sd,na.rm=T)[,2]
indexrel[,paste('qdap_negator_value_rel','_sum',sep='')]=aggregate(total[,'qdap_value'],list(as.factor(total$ym)),sum,na.rm=T)[,2]


# Inflation related articles only
total[,'qdap_negator_value_rel_infl']=total[,'qdap_value']/total[,'qdap_nword']*total$InflationMutationen
indexrel[,paste('qdap_negator_value_rel_infl','_mean',sep='')]=aggregate(total[,'qdap_negator_value_rel_infl'],list(as.factor(total$ym)),mean,na.rm=T)[,2]
nsinfl=aggregate(total$InflationMutationen==1,list(as.factor(total$ym)),sum,na.rm=T)[,2]

indexrel=indexrel[-nrow(indexrel),]
tt=ts(indexrel[1:182,1:ncol(indexrel)],start=c(1999,12),freq=12)
plot(tt)
shareinfl=nsinfl/ns*100
plot(shareinfl,type='l')
tt=total[total$year.x==2003&total$month=='05',]

write.csv(indexrel,paste(DirCode,'/data/zeit indikatoren/zeit.indexes.qdap.negator.csv',sep=''))


