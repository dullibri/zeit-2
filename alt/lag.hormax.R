lag.hormax <-
function(df,horizon,maxlag){
        # returns a lag matrix of the lags of dataframe df from lag=horizon to lag=maxlag.
        x.n=ncol(df)
        x.obs=nrow(df)
        for (lag in horizon:(horizon+maxlag-1)){
                if (lag==horizon){df.hormax=lag.exact(df,horizon)}else{
                        df.hormax=cbind(df.hormax,lag.exact(df,lag))
                        }
                }
        return(df.hormax)
        }
