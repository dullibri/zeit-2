DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
target='IP'
maxobs=60
plag=2 # publication lag
zeitraus=1 # if one, zeitindices will be ignored
library(stringr)

#         library(MCS)
resse=list()
for (h in 1:12){
#         h=8 # horizon
        auxcodedir=paste(DirCode,'/code/auxiliary code',sep='')
        source(paste(auxcodedir,'/lag.exact.R',sep=''))
        source(paste(DirCode,'/Code/Clark_West_Test/f_Newey_West_vector.r',sep='') )
        source(paste(DirCode,'/Code/Clark_West_Test/f_Clark_West_Test.r',sep='')) 
        source(paste(auxcodedir,'/olsself.R',sep=''))
        source(paste(DirCode,'/Code/giacomini rossi/FB_Wald_CI.r',sep='')) 
        # creating aggregation matrix
        firstyear=1990
        lastyear=2015
        nyear=lastyear-firstyear+1
        
        data=data.frame(m=rep(1:12,nyear)
                        ,month=rep(c(paste(0,1:9,sep=''),paste(10:12)),nyear)
                        ,year=rep(firstyear:lastyear,each=12))
        data$ym=paste(data$year,data$month,sep='-')
        data$datE=paste(data$year,data$m,sep='-')
        
        # get ecri recession dates
        ecri=read.csv(paste(DirCode,'/data/ecri/ecri.csv',sep=''),row.names=1)
        row.names(ecri)=gsub('15/','',row.names(ecri))
        t1=t(matrix(unlist(strsplit(row.names(ecri),'/')),nrow=2))
        row.names(ecri)=apply(t1,1,function(x) paste(x[2],x[1],collapse='',sep='-'))
        data[data$ym%in%row.names(ecri),'ecri']=ecri[,1]
        
        # transformation function
        target.t=function(y.raw,horizon){
                y=1200/horizon*log(y.raw/lag.exact(y.raw,horizon))
        }
        
        # get last vintage (y.fin)
        load(paste(DirCode,'/data/realtime_sets_cutoffday_31.RData',sep=''))
        set=sets[[length(sets)]]
        y.fin.col=grep(var.used[target,'code'],colnames(set))
        y.fin=set[,y.fin.col,drop=F]
        colnames(y.fin)=target
        target.untr=paste(target,'untr',sep='-')
        data[data$ym%in%row.names(y.fin),target.untr]=y.fin
        # transform the data accordingly
        data[,target]=target.t(data[,target.untr,drop=F],h)
        
        # get vintage dates and target dates
        row.n.vint=which(data$datE%in%gsub('-31','',names(sets)))
        data[row.n.vint,'vint']=1
        data[row.n.vint,'vint.num']=1:length(sets)
        data[row.n.vint+h-plag,'set-target']=1:length(sets)
        
        
        data$eval=is.na(data$'IP')==F&is.na(data$'set-target')==F
        
        target.df=data[data$eval,c(target,'set-target')]
        
        # loading results
        res.file=paste(DirCode,'/Results/IP_h',h,'_maxobs_',maxobs+h,'.RData',sep='')
        load(res.file)
        
        # forecasts
        if (zeitraus==1){
                zeitind=grep('zeit',row.names(forecast.all[[1]]))
                forecast.all=lapply(forecast.all,function(x) x=x[-zeitind,])
        }
        fc=sapply(forecast.all,function(x) as.numeric(x$fc))
        modn=row.names(forecast.all[[1]])
        # dropping vintages that can not be used
        fc=fc[,1:nrow(target.df)]
        row.names(fc)=modn
        # dimensions OK? dim(fc)
        targetm=t(matrix(rep(target.df[,target],nrow(fc)),ncol=nrow(fc)))
        
        
        fe=fc-targetm
        sfe=fe^2
        # attaching dates to the errors
        tdates=data[data$eval,'ym']
        colnames(fe)=tdates
        
        
        
        # getting basic statistics
        result.f=function(sfe){
                
                rk.period=apply(sfe,2,rank)
                result=data.frame(mse=rowMeans(sfe),
                                  rank.mean=rowMeans(rk.period)
                )
                result$rmse=result$mse^.5
                result$theilsu=result$rmse/result['ar','rmse']
                result$rank.rmse=round(rank(result$rmse),0)
                result$rank.rank.mean=round(rank(result$rank.mean),0)  
                return(result)
        }
        
        result=result.f(sfe)
        
        # recession (only 9 months)
        recession=ecri[row.names(ecri)%in%tdates,1]
        recession.ind=recession==0
        fe.r=fe[,recession.ind]
        result.r=result.f(fe.r)
        
        # Clark West
        vFE_small=fe['ar',]
        vFcst_small=fc['ar',]
        cH=h
        cw=lapply(2:nrow(fc),function(i) f_Clark_West_Test(vFE_small, fe[i,], vFcst_small, fc[i,], cH))
        result[2:nrow(fc),'cw.p']=unlist(sapply(cw,function(x)x[2]))
        
        
        plot(target.df[,'IP'],type='l')
        lines(fc[1,],col='green')
        
        plot(target.df[,'IP'],type='l')
        lines(fc['IFO.EXP',],col='green')
        
        # To do:
        # - all tests in old paper
        # - giacomini rossi
        
        
        # giacomini rossi ---------------------------------------------------------
        # ###### Dating of Forecast Breakdowns ######
        # # Surprise loss
        # Neval = ncol(fc)
        # Nmodels=nrow(fc)
        # SL.arx = sfe-rowMeans(sfe)
        # 
        # # Variances
        # sfe.demeaned=sfe-rowMeans(sfe)%*%matrix(1,nrow=1,ncol=Neval)
        # SLL.arx=cov(t(sfe.demeaned))
        # SLL.arx=diag(SLL.arx)
        # 
        # # Parameters
        # Nobs=sapply(forecast.all,function(x) x$nobs)
        # Nobs.dev=max(as.numeric(apply(Nobs,1,max))-as.numeric(apply(Nobs,1,min)))
        # if (Nobs.dev>2){stop('Nobs vary to much')}
        # min.obs=as.numeric(apply(Nobs,1,min))
        # lambdavec=min.obs*2/3
        # # lambda=2/3*(max.obs/Neval)
        # 
        # # HAC variance estimator of demeaned surprise losses
        # bw=0#floor(Neval^(1/3))# rounded down
        # SLL.arx = matrix(0,nrow=1,ncol=Nmodels)
        # for (n in 1:Nmodels){
        #         hacest<-function(sqerror.demeaned,Neval,bw){
        #                 SLL = 0
        #                 for (j in 1:bw){
        #                         Gamma = t(sqerror.demeaned[(1+j):Neval])%*%sqerror.demeaned[1:(Neval-j)]/Neval
        #                         SLL = SLL+2*(1-j/(bw+1))*Gamma
        #                 }
        #                 SLL = SLL+t(sqerror.demeaned)%*%sqerror.demeaned/Neval
        #         }
        #         SLL.arx=apply(sfe.demeaned,1,hacest,Neval,bw)
        # }
        # # Variance estimator out-of-sample losses
        # sigma2 = lambdavec*SLL.arx
        # 
        # # regression of SL on themselves
        # pmax = 12
        # 
        # surprise.BIC = matrix(NA,nrow=pmax,ncol=Nmodels)
        # for (p in 1:pmax){
        #         surprise.res=apply(SL.arx,1,olsself,p)
        #         surprise.BIC[p,]=sapply(surprise.res,function(x) x$BIC)
        # }
        # surprise.pstar = apply(surprise.BIC, 2, which.min)
        # for (n in 1:Nmodels){
        #         surprise.res[[n]]=olsself(SL.arx[n,],surprise.pstar[n])
        # }
        # 
        # surprise.coef=sapply(surprise.res,function(x) x$b)
        # surprise.regressors=sapply(surprise.res,function(x) x$Z)
        # surprise.resid=sapply(surprise.res,function(x) x$res)
        # surprise.fit=sapply(surprise.res,function(x) x$yfit)
        # 
        # # Confidence Interval
        # source('FB_Wald_CI.R')
        # surprise.CI = list()
        # for (n in 1:Nmodels){
        #         trash = FB_Wald_CI(surprise.res[[n]]$Z,surprise.res[[n]]$res,surprise.res[[n]]$b,Neval,min.obs[n],sigma2[n],1,"rolling",bw,0.05)
        #         surprise.CI[[n]]=trash$SLfitCI
        # }
        
        # saving files ------------------------------------------------------------
        
        result.mt=result[grep('zeit',row.names(result)),]
        
        
        result.mt=result[grep('MT.',row.names(result)),]
        resse[[h]]=result.mt
        write.csv(result.mt,paste(DirCode,'/results/results',target,'_',h,'_',maxobs,'.csv',sep=''))
}
mse.ind=seq(1,23,2)
cw.p.ind=seq(2,24,2)
tab=data.frame(matrix(NA,nrow=nrow(result.mt),ncol=24))
row.names(tab)=row.names(result.mt)
mser=sapply(resse,function(x) x[,c('theilsu')])
tab[,mse.ind]=mser

cw.p=sapply(resse,function(x) x[,c('cw.p')])
cw.p2=cw.p
cw.p[cw.p2>0.0549999999]=''
cw.p[cw.p2<=0.0549999999]='**'
cw.p[cw.p2<=0.0149999999]='***'
tab[,cw.p.ind]=cw.p

# mser=cbind(mser,cw.p)
# row.names(mser)=
write.csv(tab,paste(DirCode,'/results/mse_rank_all_maxobs_',maxobs,'.csv',sep=''))
