lasso.ind=function(set,y,yx,horizon,alpha=.5,s='lambda.min'){
        # alpha=1
        # s='lambda.min'
        x=cbind(yx,set)
        # getting necessary lags
        x.lag=lag.exact(x,horizon)
        cc=complete.cases(x.lag)
        x.lag=as.matrix(x.lag[cc,])
        y=y[cc,1]
#         y=y[cc]
#         fit=glmnet(x=x.lag,y=y,alpha=alpha)
        cv.fit=cv.glmnet(x.lag, y)
        
        lambda=as.matrix(cv.fit$glmnet.fit$lambda)
        if (s=='bic'){
                mse=cv.fit$cvm
                T=nrow(x)
                nvar=cv.fit$nzero
                bic=log(mse)+nvar*log(T)/T
                big.opt=which.min(bic)
                lambda.sel=lambda[big.opt]
        }
        if (s=='lambda.min')
        {
                lambda.sel=cv.fit$lambda.min
        }
        lambda.n=which(lambda==lambda.sel)
        ind.sel=as.matrix(unlist(predict(cv.fit,type='nonzero',s=lambda.sel)))
        variables=gsub(paste('L',horizon,sep=''),'',colnames(x.lag)[ind.sel])
}
