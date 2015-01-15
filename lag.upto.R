lag.upto <-
function(x.df,max.lag){
        # Returns a dataframe of the lags of x.df up to max.lag
        x.n=ncol(x.df)
        x.obs=nrow(x.df)
        x.df.lag=data.frame(matrix(NA,nrow(x.df),max.lag*x.n))
        x.names=colnames(x.df)
        for (lag in 1:max.lag){
                x.df.lag[(lag+1):nrow(x.df),(1:x.n)+x.n*(lag-1)]=x.df[1:(nrow(x.df)-lag),]
                colnames(x.df.lag)[(1:x.n)+x.n*(lag-1)]=paste(x.names,lag,sep='.L')
                }
        row.names(x.df.lag)=row.names(x.df)
        return(x.df.lag)
        }
