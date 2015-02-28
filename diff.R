diff<-function(df,times=1,to.lag=1){
        # returns the to.lag-th difference of dataframe taken times=times of df.
        # example: times=2, to.lag=1  is the second difference of adjecent observations
        lag.exact=function(x.df,lag.length){
                # Returns a dataframe of the lags of x.df with lag.length
                x.n=ncol(x.df)
                x.obs=nrow(x.df)
                x.df.lag=data.frame(matrix(NA,nrow(x.df),x.n))
                x.names=colnames(x.df)
                x.df.lag[(lag.length+1):nrow(x.df),]=x.df[1:(nrow(x.df)-lag.length),]
                rownames(x.df.lag)=rownames(x.df)
                colnames(x.df.lag)=paste(colnames(x.df),'L',lag.length,sep='')
                return(x.df.lag)
        }   
        df.diff=df
        for (i in (1:times)){
                df.diff=df.diff-lag.exact(df.diff,to.lag)   
        }
           
        colnames(df.diff)=paste(colnames(df),'_',times,'.dif_',to.lag,'.lag',sep='')
        if (times>1){colnames(df.diff)=paste(colnames(df),'_',times,'.dif_',to.lag,'.lag',sep='')}
        return(df.diff)
}

