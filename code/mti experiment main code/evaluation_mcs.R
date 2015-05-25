# DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
target='IP'
# target='CPI.EX'
max.hor=15
# wenn rec=1 und bicres=0 dann nur recursive, wenn rec=0 und bicres=1 dann mit aic
# wenn rec=0 und bicres=1 dann bic variante
# wenn post=1 nur post recession period
rec=0 
bicres=0
plag=2 # publication lag
post=0 # nur post recession

grouping=read.csv(paste(DirCode,'/data/grouping.csv',sep=''),row.names=1)
library(stringr)
library(glmulti)
# library(pracma)
# library(MCS)
source(paste(DirCode,'/code/hansen_2005_test/f_Hansen_2005_Test_statistic.R',sep=''))
source(paste(DirCode,'/code/hansen_2005_test/f_Newey_West_bootstrapable.R',sep=''))
source(paste(DirCode,'/Code/giacomini rossi/FB_Wald_CI.R',sep=''))
ni=4 # number of indicators in output


auxcodedir=paste(DirCode,'/code/auxiliary code',sep='')
source(paste(auxcodedir,'/lag.exact.R',sep=''))
source(paste(DirCode,'/Code/Clark_West_Test/f_Newey_West_vector.r',sep='') )
source(paste(DirCode,'/Code/Clark_West_Test/f_Clark_West_Test.r',sep='')) 

source(paste(DirCode,'/Code/white_test/f_White_Reality_Check.r',sep='') )
# source(paste(DirCode,'/Code/white_test/Conduct_White_Test.r',sep='') )
source(paste(DirCode,'/Code/white_test/f_Politis_Romano_Bootstrap.R',sep='') )

source(paste(auxcodedir,'/olsself.R',sep=''))
source(paste(DirCode,'/Code/giacomini rossi/FB_Wald_CI.r',sep='')) 
# creating aggregation matrix
firstyear=1990
lastyear=2015
nyear=lastyear-firstyear+1


cs=list()
mt=list()
rs=list()
cr=list()
fbs=list() #forecastbreakdowns each period
white.p=list()
Hansen.pv=matrix(NA,nrow=max.hor,ncol=1)
WT.pv=matrix(NA,nrow=max.hor,ncol=1)
for (h in 1:max.hor){# h=3 
        
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
        ecri[,1]=1
        crisis.end1='2008-12'
        crisis.1=which(row.names(ecri)==crisis.end1)
        crisis.end2='2010-06'
        crisis.2=which(row.names(ecri)==crisis.end2)
        ecri[crisis.1:crisis.2,1]=0
        data[data$ym%in%row.names(ecri),'ecri']=ecri[,1]
        
        # transformation function
        target.t=function(y.raw,horizon){
                y=1200/horizon*log(y.raw/lag.exact(y.raw,horizon))
        }
        
        # get last vintage (y.fin)
        load(paste(DirCode,'/data/realtime_sets_cutoffday_31.RData',sep=''))
        set=sets[[length(sets)]]
        row.names(var.used)=gsub('-','\\.',row.names(var.used))
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
        
        targetcol=grep(paste(target,'$',sep=''),colnames(data))
        data$eval=is.na(data[,targetcol])==F&is.na(data$'set-target')==F
        
        target.df=data[data$eval,c(target,'set-target')]
        
        
        # loading results
        # res.file=paste('H:/git/zeit-2/Results/IP_h',h,'_maxobs_',60+h,'.RData',sep='')
        if (rec==1){
                res.file=paste(DirCode,'/Results/rec_',target,'_h',h,'.RData',sep='')
        }
        if (rec==0&bicres==0){
                res.file=paste(DirCode,'/Results/aic',target,'_h',h,'.RData',sep='')  
        }
        if (rec==0&bicres==1){
                res.file=paste(DirCode,'/Results/',target,'_h',h,'.RData',sep='')
        }
        load(res.file)
        
        # forecasts
        
        fc=sapply(forecast.all,function(x) as.numeric(x$fc))
        
        modn=row.names(forecast.all[[1]])
        # dropping vintages that can not be used
        fc=fc[,1:nrow(target.df)]
        row.names(fc)=modn
        #         fc=fc[-grep('zeit|rword',row.names(fc)),]
        # dimensions OK? dim(fc)
        targetm=t(matrix(rep(target.df[,target],nrow(fc)),ncol=nrow(fc)))
        fe=fc-targetm
        # attaching dates to the errors
        tdates=data[data$eval,'ym']
        colnames(fe)=tdates
        
        if (post==1){
                endrec=tail(which(ecri==0),1)
                post.id=ecri
                post.id[1:endrec,1]=0
                post.dates=row.names(post.id)[post.id==1]
                post.fe.id=which(colnames(fe)%in%post.dates)
                fe=fe[,post.fe.id]
                fc=fc[,post.fe.id]
                
        }
        tfe=t(fe)
        # MCS ---------------------------------------------------------------------
        #         MCS=MCSprocedure(Loss=tfe^2,alpha=0.01,B=100,statistic='Tmax')
        
        #         MCS=MCSprocedure(Loss=Loss[,1:5],alpha=0.2,B=5000,statistic='Tmax')
        # Hansen test --------------------------------------------------------------
        
        
        
        #         alternative = as.matrix(tfe[, -grep('ar',colnames(tfe))])
        #         benchmark=tfe[,'ar']
        #         #         dim(benchmark) = c(nrow(tfe), 1)
        #         nalt=ncol(alternative)
        #         bbench=rep(benchmark,nalt)
        #         dim(bbench)=c(nrow(tfe),ncol=nalt)
        #         lossm=bbench^2-alternative^2
        #         #         lossm=abs(bbench)-abs(alternative)
        #         blockparam = 1/nrow(tfe) # Bootstrap block size
        #         blockparam=0.9
        #         nrep=1000
        
        
        # lossm in einzelne zeitreihen in eine liste bringen
        #         lossm.l=list()
        #         for (i in 1:ncol(lossm)){lossm.l[[i]]=lossm[,i]}
        #         Tstats.original=t(sapply(lossm.l,f_Hansen_2005_Test_statistic,blockparam))
        #         Tspa=max(c(max(Tstats.original[,1]),0))
        #         omegas=Tstats.original[,2]
        #         
        #         froof = colMeans(lossm)
        #         Vl = max(sqrt(nrow(lossm))*froof)
        #         
        #         nrep=1000
        #         Tspastar=matrix(NA,nrow=nrep)
        #         WT=matrix(NA,nrow=nrep)
        #         for (i in 1:nrep){#i=1
        #                 New=matrix(tsboot(lossm, function(x) x, R = 1, l = ncol(fe), sim = "geom")$t,nrow=nrow(lossm))
        #                 Tstats.new=colMeans(New)/omegas*sqrt(nrow(New))
        #                 Tspastar[i]=max(c(max(Tstats.new),0))  
        #                 froof = colMeans(New)
        #                 WT[i] = max(sqrt(nrow(New))*froof)
        #                 #                 New.l=list()
        #                 #                 for (j in 1:ncol(New)){New.l[[j]]=New[,j]}
        #                 #                 Tstats.new=t(sapply(New.l,f_Hansen_2005_Test_statistic,blockparam))
        #                 #                 Tspastar[i]=max(c(max(Tstats.new[,1]),0))
        #                 
        #                 
        #                 froof = colMeans(New)
        #                 WT[i] = max(sqrt(nrow(New))*froof)
        #                 
        #                 
        #         }
        #         Hansen.pv[h]=sum(Tspastar>Tspa,na.rm=T)/sum(is.na(Tspastar>Tspa)==F)
        #         WT.pv[h]=sum(WT>Vl)/nrep
        
        
        #         PValue = rbind(PValue, Out_MAE[[1]])
        #         Model = c("MSE", "MAE")
        #         Test = data.frame(Model, PValue)
        
        # lasso
        #         lasso=read.csv(paste(DirCode,'/results/lasso_ranking',target,'_h',h,'.csv',sep=''),row.names=1)
        #         
        #         row.names(lasso)[grep(paste(target,'x',sep=''),row.names(lasso))]='ar'
        #         lasso=lasso[,1:ncol(fe)]
        #         lasso=lasso/lasso
        #         fela=lasso*fe
        
        # # get elasticnet selection
        #         if (rec==1){
        #                 ela=read.csv(paste(DirCode,'/results/rec_elanet_ranking',target,'_h',h,'.csv',sep=''),row.names=1)
        #                 
        #         }
        #         if(rec==0&bicres==0){
        #                 ela=read.csv(paste(DirCode,'/results/elanet_rankingaic',target,'_h',h,'.csv',sep=''),row.names=1)
        #                 
        #         }
        #         if (rec==0&bicres==1){
        #                 ela=read.csv(paste(DirCode,'/results/elanet_ranking',target,'_h',h,'.csv',sep=''),row.names=1)
        #                 ela=ela[-grep('zeit|rword',row.names(ela)),]
        #         }
        #         
        #         
        #         row.names(ela)[grep(paste(target,'x',sep=''),row.names(ela))]='ar'
        #         
        #         if (post==1){
        #                 ela=ela[,post.fe.id]
        #         }else{
        #                 ela=ela[,1:nrow(target.df)]
        #         }
        #         
        
        
        #         
        #         ela20=ela<=20
        #         fe20=fe*ela20
        #         fe20[fe20==0]=NA
        #         
        #         
        #         ela30=ela<=30
        #         fe30=fe*ela30
        #         fe30[fe30==0]=NA
        #         
        #         ela40=ela<=40
        #         fe40=fe*ela40
        #         fe40[fe40==0]=NA
        #         
        #         ela50=ela<=50
        #         fe50=fe*ela50
        #         fe50[fe50==0]=NA
        #         
        # ela60=ela<=60
        # fe60=fe*ela60
        # fe60[fe60==0]=NA
        # 
        # ela70=ela<=70
        # fe70=fe*ela70
        # fe70[fe70==0]=NA
        
        #         ela128=ela<=128
        #         fe128=fe*ela128
        #         fe128[fe128==0]=NA
        #         
        #         ela154=ela<=154
        #         fe154=fe*ela154
        #         fe154[fe154==0]=NA
        
        # calculating combined forecast errors
        cfc=t(data.frame('median'=apply(fc,2,median)
                         ,'mean'=apply(fc,2,mean)
                         #                          ,'medianlas'=apply(fela,2,median,na.rm=T)
                         #                          ,'meanlas'=apply(fela,2,mean,na.rm=T)
                         #                          ,'median20'=apply(fe20,2,median,na.rm=T)
                         #                          ,'mean20'=apply(fe20,2,mean,na.rm=T)
                         #                          ,'median30'=apply(fe30,2,median,na.rm=T)
                         #                          ,'mean30'=apply(fe30,2,mean,na.rm=T)
                         #                          ,'median40'=apply(fe40,2,median,na.rm=T)
                         #                          ,'mean40'=apply(fe40,2,mean,na.rm=T)
                         #                          ,'median50'=apply(fe50,2,median,na.rm=T)
                         #                          ,'mean50'=apply(fe50,2,mean,na.rm=T)
                         #                          ,'median128'=apply(fe128,2,median,na.rm=T)
                         #                          ,'mean128'=apply(fe128,2,mean,na.rm=T)
                         #                          ,'median154'=apply(fe154,2,median,na.rm=T)
                         #                          ,'mean154'=apply(fe154,2,mean,na.rm=T)
        ))
        fc=rbind(fc,cfc)
        
        # calculating fe for combination schemes
        target.mat=matrix(rep(target.df[,1],nrow(cfc)),nrow=nrow(cfc),byrow=T)
        cfe=cfc-target.mat
        fe=rbind(fe,cfe)
        
        # squared errors
        sfe=fe^2
        
        # preparing data for matlab an writing to disk
        sfe.exp=sfe
        sfe.exp=sfe.exp[-grep('median|mean',row.names(sfe.exp)),]
        row.names(sfe.exp)=NULL
        colnames(sfe.exp)=NULL
        write.table(t(sfe.exp),paste(DirCode,'/results/fe_ip_rolling_aic/sfe',h,'.csv',sep=''),
                    , row.names=F,col.names=F,sep=',',qmethod='double')
        #         # attaching dates to the errors
        #         tdates=data[data$eval,'ym']
        #         colnames(fe)=tdates
        
        # getting basic statistics
        result.f=function(sfe){
                
                rk.period=apply(sfe,2,rank)
                perco.period=1-(rk.period/nrow(sfe))
                result=data.frame(mse=rowMeans(sfe)
                                  ,rank.mean=rowMeans(rk.period)
                                  ,rank.sd=apply(rk.period,1,sd)
                                  ,percom=rowMeans(perco.period)
                                  ,percosd=apply(perco.period,1,sd)
                )
                
                result$theilsu=result$mse^.5/result$mse[1]^.5
                result$rank.theilsu=round(rank(result$theilsu),0)
                result$rank.rank.mean=round(rank(result$rank.mean),0) 
                result$R=round(result$percom/result$percosd,2)
                result$Rr=nrow(sfe)-round(rank(result$R),0)+1
                return(result)
        }
        
        result=result.f(sfe)
        
        # recession (only 9 months)
        
        if (post!=1){
                recession=ecri[row.names(ecri)%in%tdates,1]
                #                 t=row.names(ecri)[tt]
                recession.ind=recession==0
                sfe.r=sfe[,recession.ind]
                result.r=result.f(sfe.r)
                cr[[h]]=result.r
                # preparing data for matlab an writing to disk
                sfe.exp=sfe.r
                sfe.exp=sfe.exp[-grep('median|mean',row.names(sfe.exp)),]
                row.names(sfe.exp)=NULL
                colnames(sfe.exp)=NULL
                write.table(t(sfe.exp),paste(DirCode,'/results/fe_ip_rolling_aic/sfer',h,'.csv',sep=''),
                            , row.names=F,col.names=F,sep=',')
                
                # Clark West during recession ---------------------------------------------
                vFE_small=fe['ar',recession.ind]
                vFcst_small=fc['ar',recession.ind]
                cH=h
                fe.r=fe[,recession.ind,drop=F]
                fc.r=fc[,recession.ind,drop=F]
                cw.r=lapply(2:nrow(fc),function(i) f_Clark_West_Test(vFE_small, fe.r[i,], vFcst_small, fc.r[i,], cH/1.5))
                #                 for (i in (2:nrow(fc))){ #vFE_big=fe.r[i,] vFcst_big=fc.r[i,]
                #                         cw.r[[i]]=f_Clark_West_Test(vFE_small, fe.r[i,], vFcst_small, fc.r[i,], cH)
                #                 }
                result.r[2:nrow(fc),'cw.p']=unlist(sapply(cw.r,function(x)x[2]))
                
        }
        # Clark West
        vFE_small=fe['ar',]
        vFcst_small=fc['ar',]
        cH=h
        cw=lapply(2:nrow(fc),function(i) f_Clark_West_Test(vFE_small, fe[i,], vFcst_small, fc[i,], cH))
        result[2:nrow(fc),'cw.p']=unlist(sapply(cw,function(x)x[2]))
        
        # 
        # plot(target.df[,'IP'],type='l')
        # lines(fc[1,],col='green')
        # 
        # plot(target.df[,'IP'],type='l')
        # lines(fc['IFO.EXP',],col='green')
        
        
        
        # giacomini rossi ---------------------------------------------------------
        ###### Dating of Forecast Breakdowns ######
        # Surprise loss
        meanmedian=grep('mean|median',row.names(sfe))
        Neval = ncol(fc)
        Nmodels=nrow(fe[-meanmedian,])
        SL.arx = sfe-rowMeans(sfe)
        #         t=rowMeans(sfe)
        
        SL.arx=SL.arx[-meanmedian,]
        #         t=rowMeans(sfe)
        # Variances
        sfe.demeaned=sfe[-meanmedian,]-rowMeans(sfe[-meanmedian,])%*%matrix(1,nrow=1,ncol=Neval)
        SLL.arx=cov(t(sfe.demeaned))
        SLL.arx=diag(SLL.arx)
        
        # Parameters
        Nobs=sapply(forecast.all,function(x) x$nobs)
        Nobs.dev=max(as.numeric(apply(Nobs,1,max))-as.numeric(apply(Nobs,1,min)))
        if (Nobs.dev>2){stop('Nobs vary to much')}
        min.obs=as.numeric(apply(Nobs,1,min))
        lambdavec=min.obs*2/3/Neval
        # lambda=2/3*(max.obs/Neval)
        
        # HAC variance estimator of demeaned surprise losses
        bw=0#floor(Neval^(1/3))# rounded down
        SLL.arx = matrix(0,nrow=1,ncol=Nmodels)
        for (n in 1:Nmodels){#n=1
                hacest<-function(sqerror.demeaned,Neval,bw){
                        SLL = 0
                        for (j in 1:bw){
                                Gamma = t(sqerror.demeaned[(1+j):Neval])%*%sqerror.demeaned[1:(Neval-j)]/Neval
                                SLL = SLL+2*(1-j/(bw+1))*Gamma
                        }
                        SLL = SLL+t(sqerror.demeaned)%*%sqerror.demeaned/Neval
                }
                SLL.arx=apply(sfe.demeaned,1,hacest,Neval,bw)
        }
        # Variance estimator out-of-sample losses
        sigma2 = lambdavec*SLL.arx
        
        # regression of SL on themselves
        p.max = 12
        
        surprise.BIC = matrix(NA,nrow=p.max,ncol=Nmodels)
        for (p in 1:p.max){
                surprise.res=apply(SL.arx,1,olsself,p)
                surprise.BIC[p,]=sapply(surprise.res,function(x) x$BIC)
        }
        surprise.pstar = apply(surprise.BIC, 2, which.min)
        for (n in 1:Nmodels){
                surprise.res[[n]]=olsself(SL.arx[n,],surprise.pstar[n])
        }
        
        surprise.coef=sapply(surprise.res,function(x) x$b)
        surprise.regressors=sapply(surprise.res,function(x) x$Z)
        surprise.resid=sapply(surprise.res,function(x) x$res)
        surprise.fit=sapply(surprise.res,function(x) x$yfit)
        
        # Confidence Interval
        fbphases=matrix(NA,nrow=Nmodels,ncol=Neval)
        surprise.CI = list()
        for (n in 1:Nmodels){#n=1
                trash = FB_Wald_CI(surprise.res[[n]]$Z,surprise.res[[n]]$res,surprise.res[[n]]$b,Neval,min.obs[n],sigma2[n],1,"rolling",bw,0.05)
                surprise.CI[[n]]=trash$SLfitCI
                aux.start=Neval-length(trash$SLfitCI)+1
                fbphases[n,aux.start:Neval]=trash$SLfitCI>0
        }
        colnames(fbphases)=colnames(sfe)
        fbs[[h]]=fbphases
        n.breakdowns=colSums(fbphases,na.rm=T)
        names(n.breakdowns)=colnames(fe)
        write.csv(fbphases,paste(DirCode,'/results/fbphases/fbphase_',h,'.csv',sep=''))
        # saving files ------------------------------------------------------------
        
        # result.mt=result[grep('zeit',row.names(result)),]
        # View(result[grep('rword|zeit',row.names(result)),])
        
        result.mt=result[grep('MT.',row.names(result)),]
        # write.csv(result.mt,paste(DirCode,'/results/results',target,h,'.csv',sep=''))
        mt[[h]]=result.mt
        result.c=result[grep('mean|median',row.names(result)),]
        cs[[h]]=result.c
        rs[[h]]=result
        
        # write.csv(result.c,paste(DirCode,'/results/results_comb_',target,h,'.csv',sep=''))
}







theil.ind=seq(1,(max.hor*ni)-1,ni)
rank.ind=seq(2,(max.hor*ni),ni)

# # combination
# theil.cs=sapply(cs,function(x) x$theilsu)
# rank.cs=sapply(cs,function(x) x$rank.theilsu)
# CS=data.frame(matrix(NA,nrow=nrow(result.c),ncol=max.hor*ni))
# CS[,theil.ind]=round(theil.cs,2)
# CS[,rank.ind]=rank.cs
# if (ni==4){
#         CS[,R.ind]=round(R.cs,2)
#         CS[,Rr.ind]=Rr.cs   
#         colnames(CS)=paste(c('theilsu_h:','rank_h:','R_h:','Rr_rank_h:'),rep(1:max.hor,each=ni),sep='')#
# }
# colnames(CS)=paste(c('theilsu_h:','rank_h:'),rep(1:max.hor,each=ni),sep='')#
# row.names(CS)=row.names(result.c)

# best models
best=data.frame(matrix(NA,nrow=max.hor,ncol=ncol(result)))
for (i in 1:max.hor){
        t=rs[[i]]
        best[i,]=t[which.min(t$theilsu),]
        best[i,'name']=row.names(t[which.min(t$theilsu),])
}


# theil.mt=sapply(mt,function(x) x$theilsu)
rank.mt=sapply(mt,function(x) x$rank.theilsu)
row.names(rank.mt)=row.names(result.mt)
cra=sapply(cr,function(x) x$rank.theilsu)
row.names(cra)=row.names(result)
cra.mt=cra[grep('MT.',row.names(cra)),]
rank.rr.mt=sapply(mt,function(x) x$Rr)
# MT=data.frame(matrix(NA,nrow=nrow(result.mt),ncol=max.hor*2))
# 
# MT[,theil.ind]=round(theil.mt,2)
# MT[,rank.ind]=rank.mt
# row.names(MT)=row.names(result.mt)
# colnames(MT)=paste(c('theilsu_h:','rank_h:'),rep(1:max.hor,each=ni),sep='')
# 
# 


# processing matlab mcs results -------------------------------------------
grouping=grouping[-grep('mean|median',row.names(grouping)),]

fres=list()
sfeorr='sfe'
for (h in 1:14){
        inc=read.csv(paste(DirCode,'/results/fe_ip_rolling_aic/includeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exc=read.csv(paste(DirCode,'/results/fe_ip_rolling_aic/excludeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exin=rbind(exc,inc)
        
        inc.ind=rep(1,nrow(exin))
        inc.ind[1:nrow(exc)]=0
        pval=read.csv(paste(DirCode,'/results/fe_ip_rolling_aic/pvalsSQ',sfeorr,h,'.csv',sep=''),header=F)
        
        mcs.h=data.frame(exin,pval,inc.ind)
        if (sfeorr=='sfe'){
                result=rs[[h]]  
        }else{
                result=cr[[h]]
        }
        
        result[,c('model.id','pval','inc')]=NA
        result[mcs.h[,1],c('model.id','pval','inc')]=mcs.h  
        fres[[h]]=result
        
}
# mt models included
MT.inc=sapply(fres,function(x) x[grep('MT',row.names(x)),'inc'])

row.names(MT.inc)=row.names(result)[grep('MT',row.names(result))]
MT.inc.sum=colSums(MT.inc)

# all models inc
fres=lapply(fres,function(x) x[-grep('mean|median',row.names(x)),])
ALL.inc=data.frame(sapply(fres,function(x) x[,'inc']))
row.names(ALL.inc)=row.names(result[-grep('mean|median',row.names(result)),])
if (sfeorr=='sfe'){
        incsfe=ALL.inc
}else{
        incsfer=ALL.inc
}
ALL.inc[,'grouping']=grouping
ALL.inc[ALL.inc$grouping=='media','media']=1
ALL.inc[ALL.inc$grouping!='media','media']=0
t=aggregate(ALL.inc[,1:14],list(ALL.inc$'media'),sum)
tall=aggregate(ALL.inc[,1:14],list(ALL.inc$'grouping'),sum)
row.names(t)=c('other','media')
row.names(tall)=tall[,1]
tall=tall[,-c(1,2,3)]
colnames(tall)=paste(1:12,sep='')
t=t[,-1]
t=as.matrix(t)
t=t[,-c(1,2)]
colnames(t)=paste(1:12,sep='')
t=t/nrow(ALL.inc)*100
if (sfeorr=='sfe'){
        pdf(paste(DirCode,'/figs/mcs_share_media_all.pdf',sep=''))
        barplot(t,legend=row.names(t)
                ,ylim=c(0,80)
                ,xlab='forecast horizon'
                ,ylab='% of models included in MCS'
        )  
        dev.off()
        write.csv(tall,paste(DirCode,'/results/mcs.all.csv',sep=''))
        
}
if (sfeorr=='sfer'){
        pdf(paste(DirCode,'/figs/mcs_share_media_recession.pdf',sep=''))
        barplot(t,legend=row.names(t)
                ,ylim=c(0,60)
                ,xlab='forecast horizon'
                ,ylab='% of models included in MCS'
        ) 
        dev.off()
        write.csv(tall,paste(DirCode,'/results/mcs.recession.csv',sep=''))
        
}



# stats for best models -------------------------------------
fres.s=fres[3:14]

# result=fres.s[[10]]
best.m=function(result,h){
        
        result=result[sort(result$rank.theilsu,index.return=T)$ix,]
        # result=result[-grep('mean|median',row.names(result)),]
        result$rank.theilsu=sort(result$rank.theilsu,index.return=T)$ix
        if (sfeorr=='sfe'){
                colnames(result)=gsub('cw.p','cw.pvalue',colnames(result))
                sel=c('rmse','theilsu','cw.pvalue','rank.theilsu','mcs.pvalue','in mcs') 
                result$cw.pvalue=round(result$cw.pvalue,2)
        }else{
                sel=c('rmse','theilsu','rank.theilsu','mcs.pvalue','in mcs')
        }
        row.names(result)=gsub('presence','present',row.names(result))
        result$mse=result$mse^.5
        colnames(result)=gsub('mse','rmse',colnames(result))
        colnames(result)=gsub('inc','in mcs',colnames(result))
        rr=result
        colnames(result)=gsub('^pval','mcs.pvalue',colnames(result))
        
        result=result[,sel]
        t=result[grep('MT',row.names(result)),]
        best=which.min(t$'rank.theilsu')
        t=t[sort(t$rank.theilsu,index.return=T)$ix,]
        t[,c('rmse','mcs.pvalue','theilsu')]=round(t[,c('rmse','mcs.pvalue','theilsu')],2)
        
        #         t=head(t,1)
        
        
        
        t[,'horizon']=h
        t=cbind(model=row.names(t),t)
        if (sfeorr=='sfe'){
                t=t[,c('model','rmse','theilsu','rank.theilsu','cw.pvalue','in mcs','mcs.pvalue')]
                
        }else{
                t=t[,c('model','rmse','theilsu','rank.theilsu','cw.pvalue','in mcs','mcs.pvalue')]
        }
        row.names(t)=NULL
        
        return(t)
}
tt=lapply(1:12,function(x) best.m(fres.s[[x]],x))



for (h in 1:12){
        if (h==1){
                besth=tt[[h]]
                besth$h=h
                t=besth[1,]
                tf=besth[besth$model=='MT.de.future',]
                tc=besth[besth$model=='MT.de.climate',]
                tp=besth[besth$model=='MT.present',]
        }else{
                besth=tt[[h]]
                besth$h=h
                t=rbind(t,besth[1,])
                tf=rbind(tf,besth[besth$model=='MT.de.future',])
                tc=rbind(tc,besth[besth$model=='MT.de.climate',])
                tp=rbind(tp,besth[besth$model=='MT.present',])
        } 
        
}
favorite=rbind(tf,tc,tp)
if (sfeorr=='sfe'){
        write.csv(t,paste(DirCode,'/results/mtbestall.csv',sep=''))
}else{
        write.csv(t,paste(DirCode,'/results/mtbestrecession.csv',sep=''))
        
}


# fbp analysis ------------------------------------------------------------

# fres=fres[[14]]
fbpall=matrix(NA,nrow=nrow(fbphases),ncol=max.hor)
row.names(fbpall)=row.names(sfe)[1:(nrow(sfe)-2)]
fbprec=matrix(NA,nrow=nrow(fbphases),ncol=max.hor)
row.names(fbprec)=row.names(sfe)[1:(nrow(sfe)-2)]

fbp.sub.all=matrix(NA,nrow=3,ncol=max.hor)
row.names(fbp.sub.all)=c('fbp MT.de.future','fbp MT.de.climate','mean')

fbp.sub.rec=matrix(NA,nrow=3,ncol=max.hor)
row.names(fbp.sub.rec)=c('fbp MT.de.future','fbp MT.de.climate','mean')

for (h in 1:14){#h=4
        t3=fbs[[h]]
        row.names(t3)=row.names(sfe)[1:(nrow(sfe)-2)]
        # fbp for all periods
        fbpall[,h]=rowMeans(t3,na.rm=T)

        rdate=data$ym[which(data$ecri==0)]
        rcol=which(colnames(t3)%in%rdate)
        fbprec[,h]=rowMeans(t3[,rcol])
        
        fbp.sub.rec['fbp MT.de.future',h]=fbprec['MT.de.future',h]
        fbp.sub.rec['fbp MT.de.climate',h]=fbprec['MT.de.climate',h]
        fbp.sub.rec['mean',h]=mean(fbprec[,h])#incsfer[,h]==1
        

        fbp.sub.all['fbp MT.de.future',h]=fbpall['MT.de.future',h]
        fbp.sub.all['fbp MT.de.climate',h]=fbpall['MT.de.climate',h]
        fbp.sub.all['mean',h]=mean(fbpall[,h])#incsfe[,h]==1
       
        
}
fbptab=matrix(NA,nrow=3,ncol=6)
row.names(fbptab)=row.names(fbp.sub.all)
fbptab[,c(1,3,5)]=fbp.sub.all[,12:14]
fbptab[,c(2,4,6)]=fbp.sub.rec[,12:14]
fbptab=round(fbptab,2)

write.csv(fbptab,paste(DirCode,'/results/fbp.csv',sep=''))

