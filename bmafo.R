bmafo <-
function(y.df,x.df,horizon,max.lag){
        # building on lag.exact, lag.hormax and plugin.values, the functions estimates a 
        # ols model for predicting # of horizons ahead with a maximum lag of max.lag.
        # requires BMA package. y.df and x.df should be of same length and complete cases.
        # The resulting forecast is row name is the row name of the last observation and
        # "H"+horizon.
        x.df.lag=lag.hormax(x.df,horizon,max.lag)
        xy.complete=complete.cases(cbind(x.df.lag,y.df))
        nobs=sum(xy.complete)
        x.df.lag=as.matrix(x.df.lag[xy.complete,])
        y=y.df[xy.complete,]
        bma.res=bicreg(x.df.lag,y)
        plugin.values=plugin.values(x.df,max.lag)
        colnames(plugin.values)=colnames(x.df.lag)
        bma.fc=predict(bma.res,newdata=plugin.values)$mean  
        names(bma.fc)=paste(row.names(y.df)[xy.complete][nobs],horizon,sep='H')
        }
