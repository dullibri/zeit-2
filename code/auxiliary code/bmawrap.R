randomforestwrapper=function(set,y,yx,horizon){
        # alpha=1
        # s='lambda.min'
        x=cbind(yx,set)
#         x=set
        # getting necessary lags
        x.lag=lag.exact(x,horizon)
        cc=complete.cases(x.lag)
        x.lag=as.matrix(x.lag[cc,])
        y=y[cc,1]
        t=randomForest(x.lag,y)
        X=as.matrix(cbind(y,x.lag))
        t=bms(X)
        bma=bicreg(x.lag,y)
        newdata=x[nrow(x),]
        fc=predict(bma,newdata,quantiles=0.5)
        return(res)
}