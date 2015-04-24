DirCode='h:/Git/zeit-2'
target='IP'
max.hor=12
plag=2 # publication lag
library(stringr)

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

cs=list()
mt=list()
rs=list()
for (h in 1:max.hor){# h=1 # horizon

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
# res.file=paste('H:/git/zeit-2/Results/IP_h',h,'_maxobs_',60+h,'.RData',sep='')
res.file=paste('H:/git/zeit-2/Results/IP_h',h,'.RData',sep='')

load(res.file)

# forecasts

fc=sapply(forecast.all,function(x) as.numeric(x$fc))

modn=row.names(forecast.all[[1]])
# dropping vintages that can not be used
fc=fc[,1:nrow(target.df)]
row.names(fc)=modn
fc=fc[-grep('zeit|rword',row.names(fc)),]
# dimensions OK? dim(fc)
targetm=t(matrix(rep(target.df[,target],nrow(fc)),ncol=nrow(fc)))

# lasso
lasso=read.csv(paste(DirCode,'/results/lasso_ranking',target,'_h',h,'.csv',sep=''),row.names=1)

row.names(lasso)[grep(paste(target,'x',sep=''),row.names(lasso))]='ar'
lasso=lasso[,1:nrow(target.df)]

# get elasticnet selection
ela=read.csv(paste(DirCode,'/results/elanet_ranking',target,'_h',h,'.csv',sep=''),row.names=1)
row.names(ela)[grep(paste(target,'x',sep=''),row.names(ela))]='ar'
ela=ela[,1:nrow(target.df)]
fe=fc-targetm



ela20=ela<=20
fe20=fe*ela20
fe20[fe20==0]=NA


ela30=ela<=30
fe30=fe*ela30
fe30[fe30==0]=NA

ela40=ela<=40
fe40=fe*ela40
fe40[fe40==0]=NA

ela50=ela<=50
fe50=fe*ela50
fe50[fe50==0]=NA

# ela60=ela<=60
# fe60=fe*ela60
# fe60[fe60==0]=NA
# 
# ela70=ela<=70
# fe70=fe*ela70
# fe70[fe70==0]=NA

ela128=ela<=128
fe128=fe*ela128
fe128[fe128==0]=NA

ela154=ela<=154
fe154=fe*ela154
fe154[fe154==0]=NA

# calculating combined forecast errors
cfe=t(data.frame('median'=apply(fe,2,median)
              ,'mean'=apply(fe,2,mean)
#               ,'median20'=apply(fe20,2,median,na.rm=T)
#               ,'mean20'=apply(fe20,2,mean,na.rm=T)
              ,'median30'=apply(fe30,2,median,na.rm=T)
              ,'mean30'=apply(fe30,2,mean,na.rm=T)
              ,'median40'=apply(fe40,2,median,na.rm=T)
              ,'mean40'=apply(fe40,2,mean,na.rm=T)
              ,'median50'=apply(fe50,2,median,na.rm=T)
              ,'mean50'=apply(fe50,2,mean,na.rm=T)
#               ,'median128'=apply(fe128,2,median,na.rm=T)
#               ,'mean128'=apply(fe128,2,mean,na.rm=T)
#               ,'median154'=apply(fe154,2,median,na.rm=T)
#               ,'mean154'=apply(fe154,2,mean,na.rm=T)
              ))
fe=rbind(fe,cfe)
sfe=fe^2
# attaching dates to the errors
tdates=data[data$eval,'ym']
colnames(fe)=tdates

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

# 
# plot(target.df[,'IP'],type='l')
# lines(fc[1,],col='green')
# 
# plot(target.df[,'IP'],type='l')
# lines(fc['IFO.EXP',],col='green')

# To do:
# - all tests in old paper
# - giacomini rossi


# # giacomini rossi ---------------------------------------------------------
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
ni=2 # number of indicators in output
theil.ind=seq(1,(max.hor*ni)-1,ni)
rank.ind=seq(2,(max.hor*ni),ni)
R.ind=seq(3,(max.hor*ni),ni)
Rr.ind=seq(4,(max.hor*ni),ni)
# combination
theil.cs=sapply(cs,function(x) x$theilsu)
rank.cs=sapply(cs,function(x) x$rank.theilsu)

R.cs=sapply(cs,function(x) x$R)
Rr.cs=sapply(cs,function(x) x$Rr)
CS=data.frame(matrix(NA,nrow=nrow(result.c),ncol=max.hor*ni))

CS[,theil.ind]=round(theil.cs,2)
CS[,rank.ind]=rank.cs
if (ni==4){
        CS[,R.ind]=round(R.cs,2)
        CS[,Rr.ind]=Rr.cs   
        colnames(CS)=paste(c('theilsu_h:','rank_h:','R_h:','Rr_rank_h:'),rep(1:max.hor,each=ni),sep='')#
        }
colnames(CS)=paste(c('theilsu_h:','rank_h:'),rep(1:max.hor,each=ni),sep='')#

row.names(CS)=row.names(result.c)

# best models
best=data.frame(matrix(NA,nrow=max.hor,ncol=ncol(result)))
for (i in 1:max.hor){
        t=rs[[i]]
        best[i,]=t[which.min(t$theilsu),]
        best[i,'name']=row.names(t[which.min(t$theilsu),])
}

# mt
# theil.mt=sapply(mt,function(x) x$theilsu)
# rank.mt=sapply(mt,function(x) x$rank.theilsu)
# MT=data.frame(matrix(NA,nrow=nrow(result.mt),ncol=max.hor*2))
# 
# MT[,theil.ind]=round(theil.mt,2)
# MT[,rank.ind]=rank.mt
# row.names(MT)=row.names(result.mt)
# colnames(MT)=paste(c('theilsu_h:','rank_h:'),rep(1:max.hor,each=ni),sep='')
# 
# 

