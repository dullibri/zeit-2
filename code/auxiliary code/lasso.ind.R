lasso.ind=function(set,y,yx,horizon,alpha=1,nlambda=1000){
        # alpha=1
        # s='lambda.min'
        x=cbind(yx,set)
        # getting necessary lags
        x.lag=lag.exact(x,horizon)
        cc=complete.cases(x.lag)
        x.lag=as.matrix(x.lag[cc,])
        y=y[cc,1]
        #         y=y[cc]
        fit=glmnet(x=x.lag,y=y,alpha=alpha,nlambda=nlambda)
        # fit=lars(x.lag,y)
        # coefm=t(as.matrix(coef(fit)))
        coefm=as.matrix(coef(fit))
        coefd=coefm
        # dropping intercept
        coefd=coefd[-1,]
        coefd[coefd!=0]=1
        coefd=coefd[rowSums(coefd)>0,]
        coefr=apply(coefd,1,function(x) min(which(x==1)))
        return(sort(coefr))
}