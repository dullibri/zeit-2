DirCode='H:/git/zeit-2'


# Loads SentiWS and splits it up in positive/negative, lower/uppercase words ----------------------------

sDirS='/SentiWS_v1.8c/'
# located in DirCode

pos.words <- read.delim(paste(DirCode,sDirS,"SentiWS_v1.8c_Positive_without_pipes.txt",sep='')# pipes have been eliminated with excel.
                        , encoding="UTF-8"
                        , header=F)

neg.words <- read.delim(paste(DirCode,sDirS,"SentiWS_v1.8c_Negative_without_pipes.txt",sep='')
                        , encoding="UTF-8"
                        , header=F)

Pos=pos.words[pos.words[,2]=='NN',]
pos=pos.words[pos.words[,2]!='NN',]

NEG=matrix(NA,nrow=nrow(neg.words),ncol=1)
for (i in 1:nrow(neg.words)){NEG[i,]=paste(neg.words[i,1],neg.words[i,4],sep=',')}
NEG=data.frame(value=neg.words[,3],words=as.character(NEG))
Neg=NEG[neg.words[,2]=='NN',]
neg=NEG[neg.words[,2]!='NN',]

POS=matrix(NA,nrow=nrow(pos.words),ncol=1)
for (i in 1:nrow(pos.words)){POS[i,]=paste(pos.words[i,1],pos.words[i,4],sep=',')}
POS=data.frame(value=pos.words[,3],words=as.character(POS))
Pos=POS[pos.words[,2]=='NN',]
pos=POS[pos.words[,2]!='NN',]

# remove double entries
doppelraus=function(x){
        x=as.character(x)
        x=strsplit(x,',')
        x=unique(x)
        x=sapply(x,function(x) x)
        
        x=paste(x,',',collapse='')
        x=gsub(' ','',x)
} 

pos[,2]=sapply(pos[,2],doppelraus)
Pos[,2]=sapply(Pos[,2],doppelraus)
neg[,2]=sapply(neg[,2],doppelraus)
Neg[,2]=sapply(Neg[,2],doppelraus)

posneg=rbind(pos,neg)
POSNEG=rbind(Pos,Neg)
# eliminating non double value words


pool=paste(posneg[,2],sep=' ',collapse=' ')
pool=sapply(strsplit(pool,','),function(x)x)
pool=gsub(' ','',pool)
dpool=duplicated(pool)
dupli=pool[dpool]
for (i in dupli){pool=gsub(i,'',posneg[,2])} # 197 terms of 28419 eliminated

pool=paste(POSNEG[,2],sep=' ',collapse=' ')
pool=sapply(strsplit(pool,','),function(x)x)
pool=gsub(' ','',pool)
dpool=duplicated(pool)
dupli=pool[dpool]
for (i in dupli){pool=gsub(i,'',POSNEG[,2])} # 2 terms of 2862 eliminated

# cleaning up
POSNEG[1,2]=gsub('<U+FEFF>','',POSNEG[1,2])
del=ls()
del=del[-c(grep('POSNEG',del),grep('posneg',del))]
rm(list=del)
rm(del)
