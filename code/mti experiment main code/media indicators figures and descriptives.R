
# this code serves as an input to "evaluation" and requires df.med --------
# DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'

# transformation function
target.t=function(y.raw,horizon){
        y=1200/horizon*log(y.raw/lag.exact(y.raw,horizon))
}
source(paste(DirCode,'/code/auxiliary code/lag.exact.R',sep=''))

# getting last vintage cpi and cutting to mt-periods
cpi=read.csv(paste(DirCode,'/data/bundesbankrealtime/M.DE.S.P.PC1.PC150.R.I.csv',sep='')
            ,stringsAsFactors=F
            ,skip=4
            ,row.names=1)

cpi=cpi[5:nrow(cpi),ncol(cpi),drop=F]
infl=target.t(cpi,12)
df.unrevised=read.csv(paste(DirCode,'/data/data.csv'
                            ,sep='')
                      ,sep=','
                      ,na.strings='NA'
                      ,row.names=1
                      ,stringsAsFactors=FALSE
)
media.mods=read.csv(paste(DirCode,'/results/full list of variables of interest with new names.csv',sep=''),stringsAsFactors=F)

df.media=df.unrevised[,c('ym',media.mods[,1])]
colnames(df.media)=c('ym',media.mods[,2])
df.media=df.media[grep('2001-01',df.media$ym):grep('2014-04',df.media$ym),]
df.media[,c('Monetary','Inflation')]=df.media[,c('Monetary','Inflation')]*100

infl=infl[row.names(infl)%in%df.media$ym,,drop=F]

# Export of descriptives --------------------------------------------------

descriptives.media.export=stargazer(df.media)
descriptives.media.export=gsub('159','160',descriptives.media.export)
writeLines(descriptives.media.export,paste(DirCode,'/tables/descriptives.media.tex',sep=''))




# cpi[,1]=as.numeric(cpi[,1])


# figures -----------------------------------------------------------------



# pdf(paste(DirCode,'/figs/cpi_crisis_breakdowns.pdf',sep=''))
par(las=2
    ,mar=c(3,1,5,3)
)

# mt.s=(mt.s-mean(mt.s)+mean(cpi[,1]))*sd(cpi[,1])/sd(mt.s)
# aa=3
# par(mfrow=c(1,1))

names.automatic=colnames(df.media)[2:6]

pdf(paste(DirCode,'/figs/automatic.indicators.pdf',sep=''))
par(mfrow=c(3,2))
for (lauf in 1:length(names.automatic)){
        plot(df.media[,names.automatic[lauf]]
             , xaxt = 'n'
             #      ,yaxt='n'
             ,ylab=''#,mt.s.name
             ,xlab=''
             ,type='l'
             #      ,xaxs = 'i'
             ,main=names.automatic[lauf]
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
        axis(side=4, at = pretty(range(infl[,1])))
        #         mtext("cpi", side=4, line=3)
        ticks=seq(1,nrow(infl),26)
        dates=gsub('-',':',row.names(infl))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
}
dev.off()

names.lamlalein=colnames(df.media)[7:12]
names.lamlalein=c('Volume','Notrend','Falling','Rising','Badrising','Otherrising')

pdf(paste(DirCode,'/figs/lamlalein.indicators.pdf',sep=''))
par(mfrow=c(3,2))
for (lauf in 1:length(names.lamlalein)){
        plot(df.media[,names.lamlalein[lauf]]
             , xaxt = 'n'
             #      ,yaxt='n'
             ,ylab=''#,mt.s.name
             ,xlab=''
             ,type='l'
             #      ,xaxs = 'i'
             ,main=names.lamlalein[lauf]
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
        axis(side=4, at = pretty(range(infl[,1])))
        #         mtext("cpi", side=4, line=3)
        ticks=seq(1,nrow(infl),26)
        dates=gsub('-',':',row.names(infl))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
}
dev.off()


names.mediatenor=colnames(df.media)[13:21]
pdf(paste(DirCode,'/figs/mediatenor.indicators.pdf',sep=''))
par(mfrow=c(3,3))
for (lauf in 1:length(names.mediatenor)){
        plot(df.media[,names.mediatenor[lauf]]
             , xaxt = 'n'
             #      ,yaxt='n'
             ,ylab=''#,mt.s.name
             ,xlab=''
             ,type='l'
             #      ,xaxs = 'i'
             ,main=names.mediatenor[lauf]
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
        axis(side=4, at = pretty(range(infl[,1])))
        #         mtext("cpi", side=4, line=3)
        ticks=seq(1,nrow(infl),26)
        dates=gsub('-',':',row.names(infl))
        axis(1, at=ticks, labels=dates[ticks]
             ,las=2
        )
}
dev.off()