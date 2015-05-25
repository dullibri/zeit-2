
# this code computes the number of observations for each mti 
# and plots the indices --------

# DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'

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
    ,mar=c(3,3,3,3)
)

# mt.s=(mt.s-mean(mt.s)+mean(ip[,1]))*sd(ip[,1])/sd(mt.s)
# aa=3
# par(mfrow=c(1,1))

for (mt.l in 2:18){
        #mt.l=2
        mt.s=data[,mt.l]
        mt.s.name=colnames(data)[mt.l] 
        pdf(paste(DirCode,'/figs/',mt.s.name,'.pdf',sep=''))
        
        plot(data[,mt.l]
             , xaxt = 'n'
             #      ,yaxt='n'
#              ,ylab=mt.s.name
             ,xlab=''
             ,type='l'
             #      ,xaxs = 'i'
             ,lty=2
        )
        par(new = TRUE)
        plot(ip[,1]
             , xaxt = 'n'
             ,yaxt='n'
             ,ylab=''
             ,xlab=''
             ,type='l'
             ,lty=1
        )
        legend('topleft'
               ,c(mt.s.name,'ip (right scale)')
               ,lty=c(1,2)
               )
        axis(side=4, at = pretty(range(ip[,1])))
#         mtext("ip", side=4, line=3)
        ticks=seq(1,nrow(ip),20)
        dates=gsub('-',':',row.names(ip))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
        dev.off()
        
}


