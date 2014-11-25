#  ------------------------------------------------------------------------
# This file prepares SentiWs creating plib and nlib  --------
#  ------------------------------------------------------------------------
# plib is a two column dataframe, the first column containing the basic forms of positive 
# words, the second column the correpsonding value. nlib is the respective df for neg. words.
# 


# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep=''))



# Getting sentiment library -----------------------------------------------

NEG=read.csv(paste(DirCode,'/','SentiWS_v1.8c','/','SentiWS_v1.8c_Negative.txt',sep=''),header=F,stringsAsFactors =F)
tt=sapply(NEG[,1],strsplit,'\\t')
tt=sapply(tt,function(x) c(x[1],x[3],x[2]))
NEG=cbind(NEG,t(tt))
rated=grep('\\|',NEG[,1])
NEG=NEG[rated,]
NEG=NEG[,1,drop=F]

wordvalue=function(x){
#         index=regexec(paste('\\|[(NN)(ADJX)(VVINF)]','\t','[:punct:]([0-9]{4})',sep=''),x)
        index=regexec(paste('(-?[0-9].[0-9]{4})|(-?[0-9].[0-9]{3}|(-?[0-9].[0-9]{2})|(-?[0-9].[0-9]{1}))',sep=''),x)
        return(regmatches(x,index)[[1]][1])
}
NEG=apply(NEG,2,as.character)
value=as.numeric(sapply(NEG,wordvalue))
word=function(x){
        index=regexec(paste('(.*)','\\|',sep=''),x)
        return(regmatches(x,index)[[1]][2])
}

basicword=sapply(NEG,word)

nlib=data.frame(basicword,value)
row.names(nlib)=NULL
rm(NEG)

POS=read.csv(paste(DirCode,'/','SentiWS_v1.8c','/','SentiWS_v1.8c_Positive.txt',sep=''),header=F)
rated=grep('\\|',POS[,1])
POS=POS[rated,]
POS=POS[,1,drop=F]

POS=apply(POS,2,as.character)
value=as.numeric(sapply(POS,wordvalue))


basicword=sapply(POS,word)

plib=data.frame(basicword,value)
row.names(plib)=NULL
rm(POS,rated,value,basicword,word,wordvalue)

pos <- Corpus(VectorSource(plib[,1]), readerControl = list(language = "de"))

pos <- tm_map(pos, tolower)
pos <- tm_map(pos, stemDocument, language = "german")
# Setting directory for stored files -------------------------------------------------------
DirRawTexts="H:/Zeit"