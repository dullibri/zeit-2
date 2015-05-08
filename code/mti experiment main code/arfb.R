t=sapply(fbs,function(x) x[1,])
tl=sapply(t,length)
arfb=data.frame(matrix(NA,ncol=tl[1],nrow=length(t)))

# using all models 20% fb
t=sapply(fbs,function(x) colSums(x,na.rm=T))
tl=sapply(t,length)
arfb=data.frame(matrix(NA,ncol=tl[1],nrow=length(t)))
colnames(arfb)=names(t[[1]])
for (i in 1:length(t)){
        arfb[i,names(t[[i]])]=t[[i]]#*(i-2)
}
arfb=arfb[-c(1,2,15),]
# plot(colSums(arfb)>171*12*0.2,type='l')


# looking at industrial production
ipstart=grep('2005-11',data$ym)
ipend=grep('2014-12',data$ym)
ipdates=data$ym[ipstart:ipend]
ips=data[ipstart:ipend,'IP-untr']

pdf(paste(DirCode,'/figs/ip_crisis_breakdowns.pdf',sep=''))
par(las=2
    ,mar=c(5.1,4,2,2.1)
)
aa=3
plot(ips
     ,ylim=c(85,119-aa)
     , xaxt = 'n'
     ,yaxt='n'
     ,ylab=''
     ,xlab=''
,type='l'
,xaxs = 'i'
)

# recession
recstart=grep('2008-05',ipdates)
recend=grep('2009-01',ipdates)
recind=1:ncol(arfb)*0
recind[recstart:recend]=1

lines(recind*(119-aa),type='p')
abline(h=118-aa
       ,lty=6
       )
text(y=(119-aa),x=90,'Recession')
# abline(h=120
#        ,lty=6
#        )
# forecast breakdowns more than 1/4 of AR-models
lines((colSums(arfb)>171*12*0.2)*(117-aa)
      ,type='p'
      ,pch=16
      )
text(y=(117-aa),x=90,'Forecast breakdowns')
abline(h=116-aa
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

axis(1, at=ticks, labels=ipdates[ticks]
          ,las=2
)
axis(2,at=c(seq(85,110,5)),c(seq(85,110,5)))
# abline(v=grep('2008-05',colnames(arfb.end)))
# abline(v=grep('2009-01',colnames(arfb.end)))
dev.off()
par(par.s)

