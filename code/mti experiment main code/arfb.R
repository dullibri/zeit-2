t=sapply(fbs,function(x) x[1,])
rownames.fbs=row.names(result)[1:(nrow(result)-2)]
for (i in 1:12){
        row.names(fbs[[i]])=rownames.fbs
}
nmod=length(sel.mods)
t=sapply(fbs,function(x) colSums(x[sel.mods,],na.rm=T))
tl=sapply(t,length)
arfb=data.frame(matrix(NA,ncol=tl[1],nrow=length(t)))
colnames(arfb)=names(t[[1]])
for (i in 1:length(t)){
        inn=colnames(arfb)[colnames(arfb)%in%names(t[[i]])]
        arfb[i,inn]=t[[i]][1:length(inn)]#*(i-2)
}
# arfb=arfb[-c(1,2,15),]
plot(colSums(arfb,na.rm=T)/nmod*max.hor*100,type='l')
instablephase=colSums(arfb,na.rm=T)>nmod*max.hor*0.20
plot(instablephase,type='l')
# plot(colSums(arfb)>171*12*0.2,type='l')


# getting target variable data --------------------------------------------
target.t=function(y.raw,horizon){
        y=1200/horizon*log(y.raw/lag.exact(y.raw,horizon))
}
source(paste(DirCode,'/code/auxiliary code/lag.exact.R',sep=''))
cpi=read.csv(paste(DirCode,'/data/bundesbankrealtime/M.DE.S.P.PC1.PC150.R.I.csv',sep='')
             ,stringsAsFactors=F
             ,skip=4
             ,row.names=1)
cpi=cpi[5:nrow(cpi),ncol(cpi),drop=F]
infl=target.t(cpi,12)
infl=infl[row.names(infl)%in%data$ym,,drop=F]


inflstart=grep('2005-11',row.names(infl))
inflend=grep('2015-01',row.names(infl))
infldates=row.names(infl)[inflstart:inflend]
ips=data[ipstart:ipend,'IP-untr']
infls=infl[inflstart:inflend,1]
maxinfls=range(infls)[2]
# pdf(paste(DirCode,'/figs/infl_crisis_breakdowns.pdf',sep=''))
par(las=2
    ,mar=c(5.1,4,2,2.1)
)
aa=0.5
plot(infls
     ,ylim=c(0,maxinfls+aa)
     , xaxt = 'n'
#      ,yaxt='n'
     ,ylab=''
     ,xlab=''
,type='l'
,xaxs = 'i'
)

# recession
recstart=grep('2008-05',infldates)
recend=grep('2009-01',infldates)
recind=1:ncol(arfb)*-2
recind[recstart:recend]=1

lines(recind*(maxinfls+0.2),type='p',bg='grey')
abline(h=maxinfls+0.1
       ,lty=6
       )
text(y=maxinfls+0.2,x=90,'Recession')
# abline(h=120
#        ,lty=6
#        )
# forecast breakdowns more than 1/5 of models
instablephase.s=instablephase[names(instablephase)%in%infldates]
lines(instablephase.s*(maxinfls+0.4)
      ,type='p'
      ,pch=16
      )
text(y=maxinfls+0.4,x=90,'Forecast breakdowns')
abline(h=maxinfls+0.5
#        ,lty=6
       )
# plot(runif(10))
# legend('bottomright'
#        ,lty=c(0,0,1)
#        ,pch=c(1,16,NA)
#        ,c('recession','forecast breakdowns','industrial production')
#        ,bty='n'
#        )
# abline(h=116)
ticks=seq(1,ncol(arfb),5)
ipdotticks=gsub('-',':',infldates)
axis(1, at=ticks, labels=ipdotticks[ticks]
          ,las=2
)
axis(2,at=c(seq(85,110,5)),c(seq(85,110,5)))
# abline(v=grep('2008-05',colnames(arfb.end)))
# abline(v=grep('2009-01',colnames(arfb.end)))
dev.off()
# par(par.s)

