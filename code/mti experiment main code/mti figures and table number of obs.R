
# this code computes the number of observations for each mti 
# and plots the indices --------

DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
# transformation function
target.t=function(y.raw,horizon){
        y=1200/horizon*log(y.raw/lag.exact(y.raw,horizon))
}
source(paste(DirCode,'/code/auxiliary code/lag.exact.R',sep=''))
data=read.csv(paste(DirCode,'/data/data.csv',sep='')
              ,stringsAsFactors=F
              ,row.names=1
              )
lamla=read.csv(paste(DirCode,'/data/mediatenor/Mediatenor_Jan2001_Jan2015.csv',sep='')
               ,stringsAsFactors=F
               ,row.names=1)

data=data[,grep('ym|qdap',colnames(data))]
data=data[complete.cases(data),]
lamla$ym=row.names(lamla)
data=merge(data,lamla,by=c('ym','ym'))
# getting last vintage cpi and cutting to mt-periods
cpi=read.csv(paste(DirCode,'/data/bundesbankrealtime/M.DE.S.P.PC1.PC150.R.I.csv',sep='')
            ,stringsAsFactors=F
            ,skip=4
            ,row.names=1)
cpi=cpi[5:nrow(cpi),ncol(cpi),drop=F]
infl=target.t(cpi,12)
infl=infl[row.names(infl)%in%data$ym,,drop=F]
# cpi[,1]=as.numeric(cpi[,1])


# figures -----------------------------------------------------------------



# pdf(paste(DirCode,'/figs/cpi_crisis_breakdowns.pdf',sep=''))
par(las=2
    ,mar=c(3,1,5,3)
)

# mt.s=(mt.s-mean(mt.s)+mean(cpi[,1]))*sd(cpi[,1])/sd(mt.s)
# aa=3
# par(mfrow=c(1,1))
names=colnames(data)[2:18]
names=gsub('presence','present',names)
names=gsub('currency','monetary',names)
data.s=data
de.id=grep('de',colnames(data))
nde.id=2:18
nde.id=nde.id[!nde.id%in%de.id]
data=data.s[,]

# pdf(paste(DirCode,'/figs/mt.de.pdf',sep=''))
par(mfrow=c(3,3))
for (mt.l in de.id){
        #mt.l=2
        mt.s=data[,mt.l]
        mt.s.name=colnames(data)[mt.l] 
        
        
        plot(data[,mt.l]
             , xaxt = 'n'
             #      ,yaxt='n'
             ,ylab=''#,mt.s.name
             ,xlab=''
             ,type='l'
             #      ,xaxs = 'i'
             ,main=names[mt.l-1]
             ,lty=1
        )
        par(new = TRUE)
        plot(infl[,1]
             , xaxt = 'n'
             ,yaxt='n'
             ,ylab=''
             ,xlab=''
             ,type='l'
             ,lty=2
        )
#         legend('topleft'
#                ,c(names[mt.l-1],'cpi (right scale)')
#                ,lty=c(1,2)
#                )
        axis(side=4, at = pretty(range(infl[,1])))
#         mtext("cpi", side=4, line=3)
        ticks=seq(1,nrow(infl),20)
        dates=gsub('-',':',row.names(infl))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
        
        
}
# dev.off()


# pdf(paste(DirCode,'/figs/mt.nde.pdf',sep=''))
par(mfrow=c(3,3))
for (mt.l in nde.id){
        #mt.l=2
        mt.s=data[,mt.l]
        mt.s.name=colnames(data)[mt.l] 
        
        
        plot(data[,mt.l]
             , xaxt = 'n'
             #      ,yaxt='n'
             ,ylab=''#,mt.s.name
             ,xlab=''
             ,type='l'
             #      ,xaxs = 'i'
             ,main=names[mt.l-1]
             ,lty=1
        )
        par(new = TRUE)
        plot(infl[,1]
             , xaxt = 'n'
             ,yaxt='n'
             ,ylab=''
             ,xlab=''
             ,type='l'
             ,lty=2
        )
        #         legend('topleft'
        #                ,c(names[mt.l-1],'cpi (right scale)')
        #                ,lty=c(1,2)
        #                )
        axis(side=4, at = pretty(range(infl[,1])))
        #         mtext("cpi", side=4, line=3)
        ticks=seq(1,nrow(infl),25)
        dates=gsub('-',':',row.names(infl))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
        
        
}
# dev.off()


