olsbic1setgetting2 <-function(y,df,horizon,max.lag,ic='bic',p1){
        # building on ols, lag.exact, lag.hormax and plugin.values, the functions estimates a 
        # ols model for predicting # of horizons ahead with a maximum lag of max.lag for up x1 and
        # x2. The optimal lag lengths p1 for x1 is given by a preceeding analyis. 
        # p2 of x2 is estimated using bic.  
        # y and df should data frames.
        # Returns: list with all central results (ols estimates, measures of fit, forecast)
        
        lag.exact<-function(df,lag.length){
                # Returns a dataframe of the lags of df with lag.length
                x.n=ncol(df)
                x.obs=nrow(df)
                df.lag=data.frame(matrix(NA,nrow(df),x.n))
                x.names=colnames(df)
                df.lag[(lag.length+1):nrow(df),]=df[1:(nrow(df)-lag.length),]
                rownames(df.lag)=rownames(df)
                colnames(df.lag)=paste(colnames(df),'L',lag.length,sep='')
                return(df.lag)
        }    
        lag.hormax=function(df,horizon,maxlag){
                # returns a lag matrix of the lags of dataframe df from lag=horizon to 
                # lag=horizon+maxlag-1.
                x.n=ncol(df)
                x.obs=nrow(df)
                for (lag in horizon:(horizon+maxlag-1)){
                        if (lag==horizon){df.hormax=lag.exact(df,horizon)}else{
                                df.hormax=cbind(df.hormax,lag.exact(df,lag))
                        }
                }
                return(df.hormax)
        }
        plugin.values<-function(df,max.lag){
                # creates the most recent values to be plugged in 
                # the regression results of lag=1 to max.lag.
                # first value will be the most recent value, last
                # max.lag-1. 
                x.obs=nrow(df)
                df.lag=lag.hormax(df,1,max.lag-1)
                plugin.values=cbind(df[x.obs,],df.lag[x.obs,])
                return(plugin.values)
        }
        ols<-function(df){
                # df: matrix/dataframe, first column containts dependent,
                # following columns contain regressors.
                # returns:
                # b: coefficient estimates, tstat and tpval t-test, f and fpval f-test, sse sum
                # of squared residuals, sst sum of squared total, sigma2 squared standard error
                # n number of observations, p number of regressors.
                
                y=df[,1]
                x=matrix(df[,2:ncol(df)],nrow=nrow(df))
                x=cbind(1,x)
                colnames(x)=c('intercept',colnames(df[,2:ncol(df),drop=F]))
                
                nobs=length(y)
                p=ncol(x)-1 # number of exogenous
                xy=t(x)%*%y
                xxi=solve(t(x)%*%x)
                h=x%*%xxi%*%t(x)
                i=diag(nobs)
                b=xxi%*%xy
                bnames=colnames(x)
                
                yhat=x%*%b
                res=y-yhat
                sst=sum((y-mean(y))^2)
                sse=t(res)%*%res
                msr=sse/nobs
                ssm=sst-sse
                
                r2=1-sse/sst
                df.e=nobs-p-1
                df.t=nobs-1
                df.m=df.t-df.e
                s2=sse/df.e
                sigma2=sse/(nobs-p)
                r2.adj=1-(sse/df.e)/(sst/df.t)
                aic=nobs*log(sse/nobs)+2*(p+1)
                #         bic=nobs*log(sse/nobs)+log(nobs)*(p+1)
                bic=nobs + nobs * log(2*pi) + nobs*log(sse/nobs) + log(nobs) * (p+1)
                f=(ssm/df.m)/(sse/df.e)
                fpval=1-pf(f,df.m,df.e)
                stde=sqrt(diag(xxi))*sqrt(s2)
                names(stde)=bnames
                tstat=b/stde
                tpval=2*(1-pt(tstat,df.e))
                res=list(b,stde,tstat,tpval,f,fpval,sse,msr,sst,aic,bic,r2,r2.adj,sigma2,nobs,p)
                names(res)=unlist(strsplit("b,stde,tstat,tpval,f,fpval,sse,msr,sst,aic,bic,r2,r2.adj,sigma2,nobs,p",',') )  
                return(res)
        }
        
        nvar=ncol(df)
        
        x1=lag.hormax(df[,1,drop=F],horizon,max.lag)
        x2=lag.hormax(df[,2,drop=F],horizon,max.lag)      
        
        
        
        # restricting to complete cases
        
        xy=cbind(y,x1,x2)
        
        xy.complete=complete.cases(xy)
        nobs=sum(xy.complete)
        x1=as.matrix(x1[xy.complete,])
        x2=as.matrix(x2[xy.complete,])
        if (sum(colSums(x2)==0)>0|(sum(colSums(x2)==nrow(x2)))>0){return()}
        
        y=y[xy.complete,]
        
        
        # getting optimal model with x2        
        
        olsaux2=function(k){
                res=ols(cbind(y,x1[,1:p1,drop=F],x2[,1:k,drop=F]))
                return(res)
        }
        sel=data.frame(1:max.lag)
        res=t(mapply(olsaux2,sel[,1]))
        p2=which.min(res[,ic])
        result=res[p2,]
        result$p2=p2
        pv1=plugin.values(df[,1,drop=F],max.lag)[1:p1]
        pv2=plugin.values(df[,2,drop=F],max.lag)[1:p2]
        pv=cbind(pv1,pv2)
        
        result$names=paste(row.names(result$b),collapse=',')
        # forecast
        latest.values=t(as.matrix(c(1,unlist(pv))))
        regressors=as.matrix(result$b)
        result$p1=p1
        result$horizon=horizon
        result$fc=latest.values%*%regressors
        return(result)
}
