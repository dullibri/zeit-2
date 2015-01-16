#  ------------------------------------------------------------------------
# This file creates a dataframe of all lowercase words in SentiWs and there values
# As it is not possible with the techniques used in the following analysis to discern between adjectives,
# adverbs, verbs, etc, there are ambigous cases that will be dropped.
#  ------------------------------------------------------------------------


DirCode='H:/git/zeit-2'
# load(paste(DirCode,"/SentiWS.RData",sep=''))

# Preparing SentiWs -------------------------------------------------------
# Loads SentiWS and splits it up in positive/negative, lower/uppercase words ----------------------------

sDirS='/SentiWS_v1.8c/'
# located in DirCode

pos.words <- read.delim(paste(DirCode,sDirS,"SentiWS_v1.8c_Positive_without_pipes.txt",sep='')# pipes have been eliminated with excel.
                        , encoding="UTF-8"
                        , header=F
                        ,stringsAsFactors=F
                        )

neg.words <- read.delim(paste(DirCode,sDirS,"SentiWS_v1.8c_Negative_without_pipes.txt",sep='')
                        , encoding="UTF-8"
                        , header=F
                        ,stringsAsFactors=F
                        )

# separating Nouns ('NN') and others
Pos=pos.words[pos.words[,2]=='NN',]
pos=pos.words[pos.words[,2]!='NN',]

NEG=matrix(NA,nrow=nrow(neg.words),ncol=1)
for (i in 1:nrow(neg.words)){NEG[i,]=paste(neg.words[i,1],neg.words[i,4],sep=',')}
NEG=data.frame(value=neg.words[,3],words=as.character(NEG),stringsAsFactors=F)
Neg=NEG[neg.words[,2]=='NN',]
neg=NEG[neg.words[,2]!='NN',]

POS=matrix(NA,nrow=nrow(pos.words),ncol=1)
for (i in 1:nrow(pos.words)){POS[i,]=paste(pos.words[i,1],pos.words[i,4],sep=',')}
POS=data.frame(value=pos.words[,3],words=as.character(POS),stringsAsFactors=F)
Pos=POS[pos.words[,2]=='NN',]
pos=POS[pos.words[,2]!='NN',]

# grouping in capital and lowercase words
posneg=rbind(pos,neg)
POSNEG=rbind(Pos,Neg)

# creating valueword: dataframe with each word having a single entry with a value
# attached ------------------------------------------------------
# first lowercase words
for (i in 1:nrow(posneg)){
        if (i==1){
        valueword=data.frame(wort=sapply(strsplit(posneg[i,2],','),function(x) x),stringsAsFactors=F)
        valueword$wert=posneg[i,1]
        }
        uu=data.frame(wort=sapply(strsplit(posneg[i,2],','),function(x) x),stringsAsFactors=F)
        uu$wert=posneg[i,1]
        valueword=rbind(valueword,uu)
}
duplo.ind=duplicated(valueword[,1])
duplo.words=valueword[duplo.ind,]
duplo.vw.ind=as.character(valueword[,1])%in%duplo.words[,1]
valueword.duplo=valueword[duplo.vw.ind,]
valueword.duplo=valueword.duplo[order(valueword.duplo[,1]),]

for (i in 2:nrow(valueword.duplo)){
        if (valueword.duplo[i,1]==valueword.duplo[i-1,1]&valueword.duplo[i,2]==valueword.duplo[i-1,2]){
                valueword.duplo[i,3]=row.names(valueword.duplo)[i]
        }
}
# rows to be deleted in valueword.duplo
duplo.el=which(is.na(valueword.duplo[,3])==F)
# eliminated from valueword.duplo and thus not eliminated subsequently from 
# valueword
valueword.duplo=valueword.duplo[-c(duplo.el),]

# rownames to be deleted in valueword
duplo.rid=as.numeric(row.names(valueword.duplo))
valueword=valueword[-duplo.rid,]

write.csv(valueword,paste(DirCode,'/valueword.csv',sep=''),row.names=F)

# capital words
for (i in 1:nrow(POSNEG)){
        if (i==1){
                VALUEWORD=data.frame(wort=sapply(strsplit(POSNEG[i,2],','),function(x) x))
                VALUEWORD$wert=POSNEG[i,1]
        }
        uu=data.frame(wort=sapply(strsplit(POSNEG[i,2],','),function(x) x))
        uu$wert=POSNEG[i,1]
        VALUEWORD=rbind(VALUEWORD,uu)
}
# duplicates? only 4, all of them exact duplicates.
DUPLO.ind=duplicated(VALUEWORD[,1])
sum(DUPLO.ind)
id=VALUEWORD[,1]%in%VALUEWORD[DUPLO.ind,1]
VALUEWORD=VALUEWORD[-which(DUPLO.ind),]
write.csv(VALUEWORD,paste(DirCode,'/valueword_capital.csv',sep=''),row.names=F)
