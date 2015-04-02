chg<-function(df,to.lag){
        # returns the to.lag-th percentage change rate of dataframe df.
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
        df.chg=(df-lag.exact(df,to.lag))/lag.exact(df,to.lag)*100
        colnames(df.chg)=paste(colnames(df),'_chg_',to.lag,sep='')
        return(df.chg)
}