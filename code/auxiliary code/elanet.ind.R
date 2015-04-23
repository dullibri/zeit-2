elanet.ind=function(set,y,yx,horizon,alpha=1,nlambda=1000){
        # alpha=1
        # s='lambda.min'
        x=cbind(yx,set)
        # getting necessary lags
        x.lag=lag.exact(x,horizon)
        cc=complete.cases(x.lag)
        x.lag=as.matrix(x.lag[cc,])
        y=y[cc,1]
        cvfit=cv.glmnet(x=x.lag,y=y,alpha=0,nlambda=nlambda)
        l2=cvfit$lambda.min
        fit=enet(x=x.lag,y=y,lambda=l2)

        # und wieder so...
        coefm=t(as.matrix(fit$beta))
        coefd=coefm
        # dropping the intercept column
        coefd=coefd[,-1]
  
        coefd[coefd!=0]=1
        coefd=coefd[rowSums(coefd)>0,]
        coefr=apply(coefd,1,function(x) min(which(x==1)))
        return(sort(coefr))
}