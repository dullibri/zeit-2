#  ------------------------------------------------------------------------
# This file creates a dataframe of all lowercase words in SentiWs and their values
# As it is not possible with the techniques used in the following analysis to discern between adjectives,
# adverbs, verbs, etc, there are ambigous cases that will be dropped.
#  ------------------------------------------------------------------------


DirCode='H:/git/zeit-2'
DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"

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
senti=rbind(pos.words,neg.words)
# merge stem and transformation of words
words=paste(senti[,1],senti[,4],sep=',')
# getting lists containing all words of individual stems
words.list=sapply(words,function(x) strsplit(x,','))
# number of words each stem
stem.n=sapply(words.list,length)

# getting single vector containing all words
words.v=character(0)
for (i in 1:length(words.list)){words.v=c(words.v,words.list[[i]])}
test=sapply(words.v,function(x) x[[1]])

valueword=data.frame(wort=words.v
        ,wert=rep(senti[,3],stem.n)
        ,stem=rep(senti[,1],stem.n)
        ,form=rep(senti[,2],stem.n)
        )

# getting duplicated words
duplo.ind=duplicated(valueword[,1])
duplo.n=sum(duplo.ind)
duplo.words=valueword[duplo.ind,]

# how many words are affected?
duplo.vw.ind=as.character(valueword[,1])%in%duplo.words[,1]
valueword.duplo=valueword[duplo.vw.ind,]
# ordering to get duplicates together
valueword.duplo=valueword.duplo[order(valueword.duplo[,1]),]
# each duplicates occurs only twice, as duplo.n*2=
duplo.n.affected=nrow(valueword.duplo)

# which duplicates have the same value?
for (i in 2:nrow(valueword.duplo)){
        if (valueword.duplo[i,1]==valueword.duplo[i-1,1]&valueword.duplo[i,2]==valueword.duplo[i-1,2]){
                valueword.duplo[i,'double']=row.names(valueword.duplo)[i]
        }
}
# rows to be deleted in valueword.duplo
duplo.el=which(is.na(valueword.duplo[,5])==F)
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
