for (i in 1:nrow(posneg)){
        if (i==1){
        valueword=data.frame(wort=sapply(strsplit(posneg[i,2],','),function(x) x))
        valueword$wert=posneg[i,1]
        }
        uu=data.frame(wort=sapply(strsplit(posneg[i,2],','),function(x) x))
        uu$wert=posneg[i,1]
        valueword=rbind(valueword,uu)
}
valueword$index=1:nrow(valueword)
