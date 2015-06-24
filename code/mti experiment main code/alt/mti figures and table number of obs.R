
# this code computes the number of observations for each mti 
# and plots the indices --------

DirCode='h:/Git/zeit-2'
# DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'

data=read.csv(paste(DirCode,'/data/data.csv',sep='')
              ,stringsAsFactors=F
              ,row.names=1)

data=data[,grep('ym|MT.',colnames(data))]
data=data[complete.cases(data),]
# getting last vintage ip and cutting to mt-periods
ip=read.csv(paste(DirCode,'/data/bundesbankrealtime/M.DE.Y.I.IP1.AA021.C.I.csv',sep='')
            ,stringsAsFactors=F
            ,row.names=1)
ip=ip[,ncol(ip),drop=F]
ip=ip[row.names(ip)%in%data$ym,,drop=F]
ip[,1]=as.numeric(ip[,1])


# figures -----------------------------------------------------------------



# pdf(paste(DirCode,'/figs/ip_crisis_breakdowns.pdf',sep=''))
par(las=2
    ,mar=c(3,1,5,3)
)

# mt.s=(mt.s-mean(mt.s)+mean(ip[,1]))*sd(ip[,1])/sd(mt.s)
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

pdf(paste(DirCode,'/figs/mt.de.pdf',sep=''))
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
        plot(ip[,1]
             , xaxt = 'n'
             ,yaxt='n'
             ,ylab=''
             ,xlab=''
             ,type='l'
             ,lty=2
        )
#         legend('topleft'
#                ,c(names[mt.l-1],'ip (right scale)')
#                ,lty=c(1,2)
#                )
        axis(side=4, at = pretty(range(ip[,1])))
#         mtext("ip", side=4, line=3)
        ticks=seq(1,nrow(ip),20)
        dates=gsub('-',':',row.names(ip))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
        
        
}
dev.off()


pdf(paste(DirCode,'/figs/mt.nde.pdf',sep=''))
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
        plot(ip[,1]
             , xaxt = 'n'
             ,yaxt='n'
             ,ylab=''
             ,xlab=''
             ,type='l'
             ,lty=2
        )
        #         legend('topleft'
        #                ,c(names[mt.l-1],'ip (right scale)')
        #                ,lty=c(1,2)
        #                )
        axis(side=4, at = pretty(range(ip[,1])))
        #         mtext("ip", side=4, line=3)
        ticks=seq(1,nrow(ip),25)
        dates=gsub('-',':',row.names(ip))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
        
        
}
dev.off()


