# DirCode='H:/git/zeit-2' # 
DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"
library(Hmisc)


ampdf=read.csv(paste(DirCode,'/data/qdap/amplifiers.csv',sep='')
               ,sep=','
               ,header=F
               ,stringsAsFactors=F
               )
ampdf=unlist(ampdf)
ampdf=ampdf[-which(ampdf=='')]
ampdf=unique(ampdf)
ampdf=sort(ampdf)
Ampdf=capitalize(ampdf)
amplifier=c(ampdf,Ampdf)
write.csv(amplifier,(paste(DirCode,'/data/qdap/amplifiers_ready.csv',sep='')))


deampdf=read.csv(paste(DirCode,'/data/qdap/deamplifiers.csv',sep='')
               ,sep=','
               ,header=F
               ,stringsAsFactors=F
)
deampdf=unlist(deampdf)
deampdf=deampdf[-which(deampdf=='')]
deampdf=unique(deampdf)
deampdf=sort(deampdf)
Deampdf=capitalize(deampdf)
deamplifier=c(deampdf,Deampdf)
write.csv(deamplifier,(paste(DirCode,'/data/qdap/deamplifiers_ready.csv',sep='')))


neg=read.csv(paste(DirCode,'/data/sentistrength_de/negators.csv',sep='')
                 ,sep=','
                 ,header=F
                 ,stringsAsFactors=F
)
neg=unlist(neg)
neg=neg[-which(is.na(neg)==T)]
neg=unique(neg)
neg=sort(neg)
Neg=capitalize(neg)
neg=c(neg,Neg)
write.csv(neg,(paste(DirCode,'/data/sentistrength_de/negators_ready.csv',sep='')))
