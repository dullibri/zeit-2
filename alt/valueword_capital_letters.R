for (i in 1:nrow(POSNEG)){
        if (i==1){
        VALUEWORD=data.frame(wort=sapply(strsplit(POSNEG[i,2],','),function(x) x))
        VALUEWORD$wert=POSNEG[i,1]
        }
        uu=data.frame(wort=sapply(strsplit(POSNEG[i,2],','),function(x) x))
        uu$wert=POSNEG[i,1]
        VALUEWORD=rbind(VALUEWORD,uu)
}

VALUEWORD=unique(VALUEWORD)
VALUEWORD$index=1:nrow(VALUEWORD)
VALUEWORD$wort=tolower(VALUEWORD$wort)
