plugin.values <-
function(df,max.lag){
        #  creates the most recent values to be plugged in 
        # the regression results of lag=1 to max.lag
        x.obs=nrow(df)
        df.lag=lag.hormax(df,1,max.lag-1)
        plugin.values=cbind(df[x.obs,],df.lag[x.obs,])
        return(plugin.values)
        }
