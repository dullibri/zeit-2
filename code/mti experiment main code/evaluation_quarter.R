DirCode='h:/Git/zeit-2'
# DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
# target='IP'
# plag=3
target='gdp'
# plag=1 # publication lag
# target='IP'
plag=0 # publication lag
max.hor=4
# wenn rec=1 und bicres=0 dann nur recursive, wenn rec=0 und bicres=1 dann mit aic
# wenn rec=0 und bicres=1 dann bic variante
# wenn post=1 nur post recession period
rec=0 
ic='aic'
bicres=0

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

# transformation function
target.t=function(y.raw,horizon){
        y=400/horizon*log(y.raw/lag.exact(y.raw,horizon))
}


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

# THE TARGET FOR EACH HORIZON
load(paste(DirCode,'/data/realtime_sets_quarterly.RData',sep=''))
nomonthlysets=names(setsq)%in%c("2005-05","2005-08","2005-11")
setsq[nomonthlysets]=NULL

naset=sapply(setsq,function(x) is.na(x[,target]))
fstf=apply(naset,2,function(x) min(which(x==T)))
# last row does not contain nas due to computation.
# fstf is the first observation, that is not available in realtime for the first, 
# the second, ... vintage:
fstf[length(fstf)]=fstf[length(fstf)-1]+1

aux=sapply(1:4,function(x) x:(length(fstf)-1))
aux=sapply(aux,function(x) x[1:length(aux[[4]])])
fstf.h=apply(aux,2,function(x) fstf[x] )

set=setsq[[length(setsq)]]
y.raw=set[,target,drop=F]
y.transformed=sapply(1:4,function(x) target.t(y.raw,x))
y.transformed=sapply(y.transformed,function(x) x)

y.h=apply(fstf.h,2,function(x) y.transformed[x,1])
nvint.evaluable=nrow(y.h)

# initializing lists
cs=list()
mt=list()
rs=list()
cr=list()
fbs=list() #forecastbreakdowns each period
white.p=list()
Hansen.pv=matrix(NA,nrow=max.hor,ncol=1)
WT.pv=matrix(NA,nrow=max.hor,ncol=1)
for (h in 1:max.hor){# h=3 
        
        # loading results
        res.file=paste(DirCode,'/Results/rolling25',ic,target,'_h',h,'.RData',sep='')  
        load(res.file)
        
        # forecasts
        aux=list()
        for (i in (1:nvint.evaluable)){
                aux[[i]]=forecast.all[[i]]
        }
        fc=sapply(aux,function(x) as.numeric(x$fc))
        na.fc=apply(fc,1,function(x) sum(is.na(x)))>0
        row.names(fc)=row.names(forecast.all[[1]])
        fc=fc[na.fc==F,]
      
        targetm=t(matrix(rep(y.h[,h],nrow(fc)),ncol=nrow(fc)))
        fe=fc-targetm
        
        
      
       
        
        
        
        # squared errors
        sfe=fe^2
        
       
        
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
        
        
        
        #         # giacomini rossi ---------------------------------------------------------
        #         ###### Dating of Forecast Breakdowns ######
        #         # Surprise loss
        #         meanmedian=grep('mean|median',row.names(sfe))
        #         Neval = ncol(fc)
        #         Nmodels=nrow(fe[-meanmedian,])
        #         SL.arx = sfe-rowMeans(sfe)
        #         #         t=rowMeans(sfe)
        #         
        #         SL.arx=SL.arx[-meanmedian,]
        #         #         t=rowMeans(sfe)
        #         # Variances
        #         sfe.demeaned=sfe[-meanmedian,]-rowMeans(sfe[-meanmedian,])%*%matrix(1,nrow=1,ncol=Neval)
        #         SLL.arx=cov(t(sfe.demeaned))
        #         SLL.arx=diag(SLL.arx)
        #         
        #         # Parameters
        #         Nobs=sapply(forecast.all,function(x) x$nobs)
        #         Nobs.dev=max(as.numeric(apply(Nobs,1,max))-as.numeric(apply(Nobs,1,min)))
        #         if (Nobs.dev>2){stop('Nobs vary to much')}
        #         min.obs=as.numeric(apply(Nobs,1,min))
        #         lambdavec=min.obs*2/3/Neval
        #         # lambda=2/3*(max.obs/Neval)
        #         
        #         # HAC variance estimator of demeaned surprise losses
        #         bw=0#floor(Neval^(1/3))# rounded down
        #         SLL.arx = matrix(0,nrow=1,ncol=Nmodels)
        #         for (n in 1:Nmodels){#n=1
        #                 hacest<-function(sqerror.demeaned,Neval,bw){
        #                         SLL = 0
        #                         for (j in 1:bw){
        #                                 Gamma = t(sqerror.demeaned[(1+j):Neval])%*%sqerror.demeaned[1:(Neval-j)]/Neval
        #                                 SLL = SLL+2*(1-j/(bw+1))*Gamma
        #                         }
        #                         SLL = SLL+t(sqerror.demeaned)%*%sqerror.demeaned/Neval
        #                 }
        #                 SLL.arx=apply(sfe.demeaned,1,hacest,Neval,bw)
        #         }
        #         # Variance estimator out-of-sample losses
        #         sigma2 = lambdavec*SLL.arx
        #         
        #         # regression of SL on themselves
        #         p.max = 12
        #         
        #         surprise.BIC = matrix(NA,nrow=p.max,ncol=Nmodels)
        #         for (p in 1:p.max){
        #                 surprise.res=apply(SL.arx,1,olsself,p)
        #                 surprise.BIC[p,]=sapply(surprise.res,function(x) x$BIC)
        #         }
        #         surprise.pstar = apply(surprise.BIC, 2, which.min)
        #         for (n in 1:Nmodels){
        #                 surprise.res[[n]]=olsself(SL.arx[n,],surprise.pstar[n])
        #         }
        #         
        #         surprise.coef=sapply(surprise.res,function(x) x$b)
        #         surprise.regressors=sapply(surprise.res,function(x) x$Z)
        #         surprise.resid=sapply(surprise.res,function(x) x$res)
        #         surprise.fit=sapply(surprise.res,function(x) x$yfit)
        #         
        #         # Confidence Interval
        #         fbphases=matrix(NA,nrow=Nmodels,ncol=Neval)
        #         surprise.CI = list()
        #         for (n in 1:Nmodels){#n=1
        #                 trash = FB_Wald_CI(surprise.res[[n]]$Z,surprise.res[[n]]$res,surprise.res[[n]]$b,Neval,min.obs[n],sigma2[n],1,"rolling",bw,0.05)
        #                 surprise.CI[[n]]=trash$SLfitCI
        #                 aux.start=Neval-length(trash$SLfitCI)+1
        #                 fbphases[n,aux.start:Neval]=trash$SLfitCI>0
        #         }
        #         colnames(fbphases)=colnames(sfe)
        #         fbs[[h]]=fbphases
        #         n.breakdowns=colSums(fbphases,na.rm=T)
        #         names(n.breakdowns)=colnames(fe)
        #         write.csv(fbphases,paste(DirCode,'/results/fbphases/fbphase_',h,'.csv',sep=''))
        #         # saving files ------------------------------------------------------------
        #         
        #         # result.mt=result[grep('zeit',row.names(result)),]
        #         # View(result[grep('rword|zeit',row.names(result)),])
        
        result.mt=result[grep('MT.',row.names(result)),]
        # write.csv(result.mt,paste(DirCode,'/results/results',target,h,'.csv',sep=''))
        mt[[h]]=result.mt
        result.c=result[grep('mean|median',row.names(result)),]
        cs[[h]]=result.c
        rs[[h]]=result
        
        # write.csv(result.c,paste(DirCode,'/results/results_comb_',target,h,'.csv',sep=''))
}

lout=read.csv(paste(DirCode,'/results/evaluation_leave_out.csv',sep=''),stringsAsFactors=F,header=F)
lout=unlist(lout)
lin=which(!row.names(tt)%in%lout)
t=sapply(rs,function(x) x[lin,'rank.theilsu'])
row.names(t)=row.names(result[lin,,drop=F])

nowcast.needed=sum(plag>0)
now.id=plag+1
if (nowcast.needed>0){
        range=((plag):(plag+12))
        horout=0:12
}else{range=now.id:(now.id+11)
      horout=1:12}
t=t[,range]
t=t[-176,]
best.ind=apply(t,2,which.min)

out=data.frame(horizon=horout,best=sapply(best.ind,function(x) row.names(t)[x]))
write.csv(out,paste(res.file,'BEST.csv',sep=''))

write.csv(t,paste(res.file,'Auswertung.csv',sep=''))





# 
# 
# theil.ind=seq(1,(max.hor*ni)-1,ni)
# rank.ind=seq(2,(max.hor*ni),ni)
# 
# # # combination
# # theil.cs=sapply(cs,function(x) x$theilsu)
# # rank.cs=sapply(cs,function(x) x$rank.theilsu)
# # CS=data.frame(matrix(NA,nrow=nrow(result.c),ncol=max.hor*ni))
# # CS[,theil.ind]=round(theil.cs,2)
# # CS[,rank.ind]=rank.cs
# # if (ni==4){
# #         CS[,R.ind]=round(R.cs,2)
# #         CS[,Rr.ind]=Rr.cs   
# #         colnames(CS)=paste(c('theilsu_h:','rank_h:','R_h:','Rr_rank_h:'),rep(1:max.hor,each=ni),sep='')#
# # }
# # colnames(CS)=paste(c('theilsu_h:','rank_h:'),rep(1:max.hor,each=ni),sep='')#
# # row.names(CS)=row.names(result.c)
# 
# # best models
# best=data.frame(matrix(NA,nrow=max.hor,ncol=ncol(result)))
# for (i in 1:max.hor){
#         t=rs[[i]]
#         best[i,]=t[which.min(t$theilsu),]
#         best[i,'name']=row.names(t[which.min(t$theilsu),])
# }
# 
# 
# # theil.mt=sapply(mt,function(x) x$theilsu)
# rank.mt=sapply(mt,function(x) x$rank.theilsu)
# row.names(rank.mt)=row.names(result.mt)
# cra=sapply(cr,function(x) x$rank.theilsu)
# row.names(cra)=row.names(result)
# cra.mt=cra[grep('MT.',row.names(cra)),]
# rank.rr.mt=sapply(mt,function(x) x$Rr)
# # MT=data.frame(matrix(NA,nrow=nrow(result.mt),ncol=max.hor*2))
# # 
# # MT[,theil.ind]=round(theil.mt,2)
# # MT[,rank.ind]=rank.mt
# # row.names(MT)=row.names(result.mt)
# # colnames(MT)=paste(c('theilsu_h:','rank_h:'),rep(1:max.hor,each=ni),sep='')
# # 
# # 
# 
# 
# # processing matlab mcs results -------------------------------------------
# grouping=grouping[-grep('mean|median',row.names(grouping)),]
# 
# fres=list()
# sfeorr='sfe'
# # alternative 'sfer' for recession
# for (h in 1:14){
#         inc=read.csv(paste(DirCode,'/results/fe_ip_rolling_aic/includeSQ',sfeorr,h,'.csv',sep=''),header=F)
#         exc=read.csv(paste(DirCode,'/results/fe_ip_rolling_aic/excludeSQ',sfeorr,h,'.csv',sep=''),header=F)
#         exin=rbind(exc,inc)
#         
#         inc.ind=rep(1,nrow(exin))
#         inc.ind[1:nrow(exc)]=0
#         pval=read.csv(paste(DirCode,'/results/fe_ip_rolling_aic/pvalsSQ',sfeorr,h,'.csv',sep=''),header=F)
#         
#         mcs.h=data.frame(exin,pval,inc.ind)
#         if (sfeorr=='sfe'){
#                 result=rs[[h]]  
#         }else{
#                 result=cr[[h]]
#         }
#         
#         result[,c('model.id','pval','inc')]=NA
#         result[mcs.h[,1],c('model.id','pval','inc')]=mcs.h  
#         fres[[h]]=result
#         
# }
# # mt models included
# MT.inc=sapply(fres,function(x) x[grep('MT',row.names(x)),'inc'])
# 
# row.names(MT.inc)=row.names(result)[grep('MT',row.names(result))]
# MT.inc.sum=colSums(MT.inc)
# 
# # all models inc
# fres=lapply(fres,function(x) x[-grep('mean|median',row.names(x)),])
# ALL.inc=data.frame(sapply(fres,function(x) x[,'inc']))
# row.names(ALL.inc)=row.names(result[-grep('mean|median',row.names(result)),])
# if (sfeorr=='sfe'){
#         incsfe=ALL.inc
# }else{
#         incsfer=ALL.inc
# }
# ALL.inc[,'grouping']=grouping
# ALL.inc[ALL.inc$grouping=='media','media']=1
# ALL.inc[ALL.inc$grouping!='media','media']=0
# t=aggregate(ALL.inc[,1:14],list(ALL.inc$'media'),sum)
# tall=aggregate(ALL.inc[,1:14],list(ALL.inc$'grouping'),sum)
# row.names(t)=c('other','media')
# row.names(tall)=tall[,1]
# tall=tall[,-c(1,2,3)]
# colnames(tall)=paste(1:12,sep='')
# t=t[,-1]
# t=as.matrix(t)
# t=t[,-c(1,2)]
# colnames(t)=paste(1:12,sep='')
# t=t/nrow(ALL.inc)*100
# if (sfeorr=='sfe'){
#         pdf(paste(DirCode,'/figs/mcs_share_media_all.pdf',sep=''))
#         barplot(t,legend=row.names(t)
#                 ,ylim=c(0,80)
#                 ,xlab='forecast horizon'
#                 ,ylab='% of models included in MCS'
#         )  
#         dev.off()
#         write.csv(tall,paste(DirCode,'/results/mcs.all.csv',sep=''))
#         
# }
# if (sfeorr=='sfer'){
#         pdf(paste(DirCode,'/figs/mcs_share_media_recession.pdf',sep=''))
#         barplot(t,legend=row.names(t)
#                 ,ylim=c(0,60)
#                 ,xlab='forecast horizon'
#                 ,ylab='% of models included in MCS'
#         ) 
#         dev.off()
#         write.csv(tall,paste(DirCode,'/results/mcs.recession.csv',sep=''))
#         
# }
# 
# 
# 
# # stats for best models -------------------------------------
# fres.s=fres[3:14]
# 
# # result=fres.s[[10]]
# best.m=function(result,h){
#         
#         result=result[sort(result$rank.theilsu,index.return=T)$ix,]
#         # result=result[-grep('mean|median',row.names(result)),]
#         result$rank.theilsu=sort(result$rank.theilsu,index.return=T)$ix
#         if (sfeorr=='sfe'){
#                 colnames(result)=gsub('cw.p','cw.pvalue',colnames(result))
#                 sel=c('rmse','theilsu','cw.pvalue','rank.theilsu','mcs.pvalue','in mcs') 
#                 result$cw.pvalue=round(result$cw.pvalue,2)
#         }else{
#                 sel=c('rmse','theilsu','rank.theilsu','mcs.pvalue','in mcs')
#         }
#         row.names(result)=gsub('presence','present',row.names(result))
#         result$mse=result$mse^.5
#         colnames(result)=gsub('mse','rmse',colnames(result))
#         colnames(result)=gsub('inc','in mcs',colnames(result))
#         rr=result
#         colnames(result)=gsub('^pval','mcs.pvalue',colnames(result))
#         
#         result=result[,sel]
#         t=result[grep('MT',row.names(result)),]
#         best=which.min(t$'rank.theilsu')
#         t=t[sort(t$rank.theilsu,index.return=T)$ix,]
#         t[,c('rmse','mcs.pvalue','theilsu')]=round(t[,c('rmse','mcs.pvalue','theilsu')],2)
#         
#         #         t=head(t,1)
#         
#         
#         
#         t[,'horizon']=h
#         t=cbind(model=row.names(t),t)
#         if (sfeorr=='sfe'){
#                 t=t[,c('model','rmse','theilsu','rank.theilsu','cw.pvalue','in mcs','mcs.pvalue')]
#                 
#         }else{
#                 t=t[,c('model','rmse','theilsu','rank.theilsu','cw.pvalue','in mcs','mcs.pvalue')]
#         }
#         row.names(t)=NULL
#         
#         return(t)
# }
# tt=lapply(1:12,function(x) best.m(fres.s[[x]],x))
# 
# 
# # 3 best all models recesion and all  -------------------------------------
# 
# best.3=function(result,h){
#         if ('mean'%in%row.names(result)){
#                 result=result[-grep('mean|median',row.names(result)),]
#         }
#         
#         result=result[sort(result$rank.theilsu,index.return=T)$ix,]
#         # result=result[-grep('mean|median',row.names(result)),]
#         result$rank.theilsu=sort(result$rank.theilsu,index.return=T)$ix
#         colnames(result)=gsub('cw.p','cw.pvalue',colnames(result))
#         result$cw.pvalue=round(result$cw.pvalue,2)
#         
#         row.names(result)=gsub('presence','present',row.names(result))
#         result$mse=result$mse^.5
#         colnames(result)=gsub('mse','rmse',colnames(result))
#         colnames(result)=gsub('inc','in mcs',colnames(result))
#         rr=result
#         colnames(result)=gsub('^pval','mcs.pvalue',colnames(result))
#         
#         t=result[1:3,]
#         t[,c('rmse','mcs.pvalue','theilsu')]=round(t[,c('rmse','mcs.pvalue','theilsu')],2)
#         
#         #         t=head(t,1)
#         
#         
#         
#         t[,'horizon']=h
#         t=cbind(model=row.names(t),t)
#         if (sfeorr=='sfe'){
#                 t=t[,c('model','rmse','theilsu','rank.theilsu','cw.pvalue','in mcs','mcs.pvalue')]
#                 
#         }else{
#                 t=t[,c('model','rmse','theilsu','rank.theilsu','cw.pvalue','in mcs','mcs.pvalue')]
#         }
#         row.names(t)=NULL
#         
#         return(t)
# }
# tt3=lapply(1:12,function(x) best.3(fres.s[[x]],x))
# result3=tt3[[1]]
# for (i in 2:12){
#         result3=rbind(result3,tt3[[i]])
# }
# write.csv(result3,paste(DirCode,'/results/best3 ',sfeorr,'.csv',sep=''))
# #  ------------------------------------------------------------------------
# 
# 
# for (h in 1:12){
#         if (h==1){
#                 besth=tt[[h]]
#                 besth$h=h
#                 t=besth[1,]
#                 tf=besth[besth$model=='MT.de.future',]
#                 tc=besth[besth$model=='MT.de.climate',]
#                 tp=besth[besth$model=='MT.present',]
#         }else{
#                 besth=tt[[h]]
#                 besth$h=h
#                 t=rbind(t,besth[1,])
#                 tf=rbind(tf,besth[besth$model=='MT.de.future',])
#                 tc=rbind(tc,besth[besth$model=='MT.de.climate',])
#                 tp=rbind(tp,besth[besth$model=='MT.present',])
#         } 
#         
# }
# favorite=rbind(tf,tc,tp)
# if (sfeorr=='sfe'){
#         write.csv(t,paste(DirCode,'/results/mtbestall.csv',sep=''))
# }else{
#         write.csv(t,paste(DirCode,'/results/mtbestrecession.csv',sep=''))
#         
# }
# 
# 
# # fbp analysis ------------------------------------------------------------
# 
# # fres=fres[[14]]
# fbpall=matrix(NA,nrow=nrow(fbphases),ncol=max.hor)
# row.names(fbpall)=row.names(sfe)[1:(nrow(sfe)-2)]
# fbprec=matrix(NA,nrow=nrow(fbphases),ncol=max.hor)
# row.names(fbprec)=row.names(sfe)[1:(nrow(sfe)-2)]
# 
# fbp.sub.all=matrix(NA,nrow=3,ncol=max.hor)
# row.names(fbp.sub.all)=c('fbp MT.de.future','fbp MT.de.climate','mean')
# 
# fbp.sub.rec=matrix(NA,nrow=3,ncol=max.hor)
# row.names(fbp.sub.rec)=c('fbp MT.de.future','fbp MT.de.climate','mean')
# 
# for (h in 1:14){#h=4
#         t3=fbs[[h]]
#         row.names(t3)=row.names(sfe)[1:(nrow(sfe)-2)]
#         # fbp for all periods
#         fbpall[,h]=rowMeans(t3,na.rm=T)
#         
#         rdate=data$ym[which(data$ecri==0)]
#         rcol=which(colnames(t3)%in%rdate)
#         fbprec[,h]=rowMeans(t3[,rcol])
#         
#         fbp.sub.rec['fbp MT.de.future',h]=fbprec['MT.de.future',h]
#         fbp.sub.rec['fbp MT.de.climate',h]=fbprec['MT.de.climate',h]
#         fbp.sub.rec['mean',h]=mean(fbprec[,h])#incsfer[,h]==1
#         
#         
#         fbp.sub.all['fbp MT.de.future',h]=fbpall['MT.de.future',h]
#         fbp.sub.all['fbp MT.de.climate',h]=fbpall['MT.de.climate',h]
#         fbp.sub.all['mean',h]=mean(fbpall[,h])#incsfe[,h]==1
#         
#         
# }
# fbptab=matrix(NA,nrow=3,ncol=6)
# row.names(fbptab)=row.names(fbp.sub.all)
# fbptab[,c(1,3,5)]=fbp.sub.all[,12:14]
# fbptab[,c(2,4,6)]=fbp.sub.rec[,12:14]
# fbptab=round(fbptab,2)
# 
# write.csv(fbptab,paste(DirCode,'/results/fbp.csv',sep=''))
# 
# 
# # desriptives -------------------------------------------------------------
# 
# td=rbind(
#         apply(data.s[,2:18],2,mean)
#         ,apply(data.s[,2:18],2,sd)
# )
# td=t(td)
# colnames(td)=c('mean','standard deviation')
# row.names(td)=gsub('currency','monetary',row.names(td))
# row.names(td)=gsub('presence','present',row.names(td))
# td=round(td,2)
# write.csv(td,paste(DirCode,'/results/mtdescriptives.csv',sep=''))
