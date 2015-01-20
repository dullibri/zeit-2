# This function returns a vector(N) of unambigous 
# ratings out of a data.frame(N,3) of 3 annotators. A 
# rating is considered unambigous, if at least 2
# of the annotators give the same rating. Possible
# values are automatically identified.

# vectorizing data.frame and getting possible 
# values excluding "NAs"
unambigous<-function(df.annotator){
        df.annotator.v=c(as.matrix(df.annotator))
        values=unique(df.annotator.v)
        values=values[is.na(values)==F]
        values.freq=sapply(values,function(x) rowSums(df.annotator==x))
        values.id=values.freq>1
        unambigous=matrix(NA,nrow=nrow(values.id),ncol=1)
        for (i in 1:nrow(unambigous)){
                val=values[values.id[i,]] 
                if (length(val)==0){unambigous[i,1]=NA}else{unambigous[i,1]=val}
                
        }
        return(unambigous)
}


