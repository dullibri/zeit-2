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
        newdata=x[nrow(x),]
        fc=predict(t,newdata)
        return(res)
}