DirCode='h:/Git/zeit-2'
# DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
# target='IP'
# plag=3
# number of vintages from start that will be used
max.evaluable=100
target='CPI.EX'
# plag=1 # publication lag
# target='IP'
plag=0 # publication lag
max.hor=12
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
library(stargazer)
# library(pracma)
# library(MCS)
source(paste(DirCode,'/code/tests/hansen_2005_test/f_Hansen_2005_Test_statistic.R',sep=''))
source(paste(DirCode,'/code/tests/hansen_2005_test/f_Newey_West_bootstrapable.R',sep=''))
source(paste(DirCode,'/Code/tests/giacomini rossi/FB_Wald_CI.R',sep=''))
ni=4 # number of indicators in output


auxcodedir=paste(DirCode,'/code/auxiliary code',sep='')
source(paste(auxcodedir,'/lag.exact.R',sep=''))
source(paste(auxcodedir,'/renumber.R',sep=''))
source(paste(auxcodedir,'/renumber.s.R',sep=''))
source(paste(DirCode,'/Code/tests/Clark_West_Test/f_Newey_West_vector.r',sep='') )
source(paste(DirCode,'/Code/tests/Clark_West_Test/f_Clark_West_Test.r',sep='')) 

source(paste(DirCode,'/Code/tests/white_test/f_White_Reality_Check.r',sep='') )
# source(paste(DirCode,'/Code/white_test/Conduct_White_Test.r',sep='') )
source(paste(DirCode,'/Code/tests/white_test/f_Politis_Romano_Bootstrap.R',sep='') )

source(paste(auxcodedir,'/olsself.R',sep=''))
source(paste(DirCode,'/Code/tests/giacomini rossi/FB_Wald_CI.r',sep=''))

# transformation function
target.t=function(y.raw,horizon){
        y=1200/horizon*log(y.raw/lag.exact(y.raw,horizon))
}

# getting variable overview
overview=read.csv(paste(DirCode,'/data/metadata.csv',sep=''),row.names=1)
row.names(overview)=gsub('-','\\.',row.names(overview))
# creating aggregation matrix
firstyear=1990
lastyear=2015
nyear=lastyear-firstyear+1

# disregarding some models
res.file=paste(DirCode,'/Results/rolling59',ic,target,'_h',1,'.RData',sep='')  
load(res.file)
mods.all=row.names(forecast.all[[1]])
rm(forecast.all)
lout=read.csv(paste(DirCode,'/results/evaluation_leave_out.csv',sep=''),stringsAsFactors=F,header=F)
lout=unlist(lout)
lin=which(!mods.all%in%lout)
sel.mods=mods.all[lin]

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
        #         ecri[,1]=1
        #         crisis.end1='2008-05'
        #         crisis.1=which(row.names(ecri)==crisis.end1)
        #         crisis.end2='2010-06'
        #         crisis.2=which(row.names(ecri)==crisis.end2)
        # ecri[crisis.1:crisis.2,1]=0
        data[data$ym%in%row.names(ecri),'ecri']=ecri[,1]
        
        
        
        # get last vintage (y.fin)
        load(paste(DirCode,'/data/realtime_sets_cutoffday_31.RData',sep=''))
        df.unrevised=read.csv(paste(DirCode,'/data/data.csv'
                                    ,sep='')
                              ,sep=','
                              ,na.strings='NA'
                              ,row.names=1
                              ,stringsAsFactors=FALSE
        )
        set=sets[[length(sets)]]
        var.used=overview[overview$Source=='Buba RTDB'&overview$used==1,]
        y.fin.col=grep(var.used[target,'code'],colnames(set))
        
        # unrevised data
        unrevised=sum(is.na(y.fin.col)==T)>0
        if (unrevised==F){
                y.fin=set[,y.fin.col,drop=F]
        }else{
                y.fin=df.unrevised[,target,drop=F]
                row.names(y.fin)=df.unrevised$ym
        }
        colnames(y.fin)=target
        target.untr=paste(target,'untr',sep='-')
        if (unrevised==F){
                data[data$ym%in%row.names(y.fin),target.untr]=y.fin
        }else{
                y.fin=y.fin[row.names(y.fin)%in%data$ym,1,drop=F]
                data[data$ym%in%row.names(y.fin),target.untr]=y.fin
        }
        
        # transform the data accordingly
        data[,target]=target.t(data[,target.untr,drop=F],h)
        
        # get vintage dates and target dates
        row.n.vint=which(data$datE%in%gsub('-31','',names(sets)))
        data[row.n.vint,'vint']=1
        data[row.n.vint,'vint.num']=1:length(sets)
        data[row.n.vint+h-plag,'set-target']=1:length(sets)
        
        # which are the actual values of the forecasts each iteration
        targetcol=grep(paste(target,'$',sep=''),colnames(data))
        data$eval=is.na(data[,targetcol])==F&is.na(data$'set-target')==F
        
        target.df=data[data$eval,c(target,'set-target')]
        
        
        
        # loading results
        res.file=paste(DirCode,'/Results/rolling59',ic,target,'_h',h,'.RData',sep='')  
        load(res.file)
        
        #         # disregard forecasts that can not YET be checked. (102=2014:5)
        #         weg=102:110
        #         for (i in weg){
        #                 forecast.all[[i]]=NULL
        #         }
        forecast.all=lapply(1:max.evaluable,function(x) forecast.all[[x]])
        fc=sapply(forecast.all,function(x) as.numeric(x$fc))
        
        modn=row.names(forecast.all[[1]])
        
        # dropping vintages that can not be used
        # max.evaluable=nrow(target.df)
        
        fc=fc[,1:min(c(max.evaluable,ncol(fc)))]
        max.eval=min(c(max.evaluable,ncol(fc)))
        row.names(fc)=modn
        # fc=fc[-grep('zeit|rword',row.names(fc)),]
        # dimensions OK? dim(fc)
        targetm=t(matrix(rep(target.df[1:max.eval,target],nrow(fc)),ncol=nrow(fc)))
        fe=fc-targetm
        # attaching dates to the errors (dates of target values not of sets)
        tdates=data[data$eval,'ym']
        tdates=tdates[1:max.eval]
        colnames(fe)=tdates
        
        
        tfe=t(fe)
        
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
        target.mat=matrix(rep(target.df[1:max.eval,1],nrow(cfc)),nrow=nrow(cfc),byrow=T)
        cfe=cfc-target.mat
        fe=rbind(fe,cfe)
        
        # squared errors
        sfe=fe^2
        
        # preparing data for matlab an writing to disk
        sfe.exp=sfe[lin,]
        row.names(sfe.exp)=NULL
        colnames(sfe.exp)=NULL
        recession=ecri[row.names(ecri)%in%tdates,1]
        recession.ind=recession==0
        sfe.exp.rec=sfe.exp[,recession.ind]
        write.table(t(sfe.exp),paste(DirCode,'/results/fe_infl_rolling_aic/sfe',h,'.csv',sep=''),
                    , row.names=F,col.names=F,sep=',',qmethod='double')
        write.table(t(sfe.exp.rec),paste(DirCode,'/results/fe_infl_rolling_aic/sfer',h,'.csv',sep=''),
                    , row.names=F,col.names=F,sep=',')
        
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
                result.r=result.f(sfe[,recession.ind])
                
                # Clark West during recession ---------------------------------------------
                vFE_small=fe['ar',recession.ind]
                vFcst_small=fc['ar',recession.ind]
                cH=h
                fe.r=fe[,recession.ind,drop=F]
                fc.r=fc[,recession.ind,drop=F]
                # ACHTUNG HIER MIN EINGESETZT
                cw.r=lapply(2:nrow(fc),function(i) f_Clark_West_Test(vFE_small, fe.r[i,], vFcst_small, fc.r[i,], min(cH/1.5,4)))
                #                 for (i in (2:nrow(fc))){ #vFE_big=fe.r[i,] vFcst_big=fc.r[i,]
                #                         cw.r[[i]]=f_Clark_West_Test(vFE_small, fe.r[i,], vFcst_small, fc.r[i,], cH)
                #                 }
                result.r[2:nrow(fc),'cw.p']=unlist(sapply(cw.r,function(x)x[2]))
                cr[[h]]=result.r
        }
        # Clark West
        vFE_small=fe['ar',]
        vFcst_small=fc['ar',]
        cH=h
        cw=lapply(2:nrow(fc),function(i) f_Clark_West_Test(vFE_small, fe[i,], vFcst_small, fc[i,], cH))
        result[2:nrow(fc),'cw.p']=unlist(sapply(cw,function(x)x[2]))
        result[2:nrow(fc),'cw.t']=unlist(sapply(cw,function(x)x[1]))
        # 
        # plot(target.df[,'IP'],type='l')
        # lines(fc[1,],col='green')
        # 
        # plot(target.df[,'IP'],type='l')
        # lines(fc['IFO.EXP',],col='green')
        
        
        
        #         # giacomini rossi ---------------------------------------------------------
        #         ###### Dating of Forecast Breakdowns ######
        #         # Surprise loss
        #         meanmedian=grep('^mean|^median',row.names(sfe))
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
        # result.mt=result[grep('zeit',row.names(result)),]
        # View(result[grep('rword|zeit',row.names(result)),])
        
        result.mt=result[grep('MT.',row.names(result)),]
        # write.csv(result.mt,paste(DirCode,'/results/results',target,h,'.csv',sep=''))
        mt[[h]]=result.mt
        result.c=result[grep('^mean|^median',row.names(result)),]
        cs[[h]]=result.c
        rs[[h]]=result
        
        # write.csv(result.c,paste(DirCode,'/results/results_comb_',target,h,'.csv',sep=''))
}




# media models ----------------------------------
media.mods=read.csv(paste(DirCode,'/results/full list of variables of interest with new names.csv',sep=''),stringsAsFactors=F)
models.in.tables=read.csv(paste(DirCode,'/results/present in tables.csv',sep=''),header=F,stringsAsFactors=F)


t.rtu=sapply(rs,function(x) x[lin,c('rank.theilsu')])
t.rtu=renumber(t.rtu)
t.min.once.3=rowSums(t.rtu<4)>1

t.tu=sapply(rs,function(x) x[lin,c('theilsu')])
t.cw=sapply(rs,function(x) x[lin,c('cw.p')])
t.cw.t=sapply(rs,function(x) x[lin,c('cw.t')])
t.cw.star_5perc.ind=(t.cw.t)>0&(t.cw<=0.05)
t.cw.star_1perc.ind=(t.cw.t)>0&(t.cw<=0.01)
t.cw.star=matrix(' ',nrow=nrow(t.cw),ncol=ncol(t.cw))
t.cw.star[t.cw.star_5perc.ind]='*'
t.cw.star[t.cw.star_1perc.ind]='**'
t.tot=cbind(t.tu,t.cw,t.rtu)
t.tot=t.tot/t.tot*-999
tu.ind=seq(1,34,3)
t.tot[,tu.ind]=round(t.tu,3)
t.tot[,tu.ind+1]=t.cw.star
t.tot[,tu.ind+2]=paste('(',t.rtu,')',sep='')
row.names(t.tot)=row.names(result)[lin]
row.names(t.tu)=row.names(result)[lin]
# renaming some variables -------------------------------------------------

old.names=c(media.mods[,1],paste('D',media.mods[,1],sep=''))
new.names=c(media.mods[,2],paste('D',media.mods[,2],sep=''))
old.id.tot=sapply(c(1:length(old.names)),function(x) which(row.names(t.tot)%in%old.names[x]))
View(data.frame(old=row.names(t.tot[old.id.tot,]),new=new.names))
row.names(t.tot)[old.id.tot]=new.names

models.in.tables=gsub('Zeit $','Zeit',models.in.tables[,1])
table1=t.tot[models.in.tables,]

t.tot.min.once.3=t.tot[t.min.once.3,]
write.csv(t.tot,paste(DirCode,'/tables/Tab_media_models_theilsu_rank_cwstars_all_periods.csv',sep=''))
write.csv(t.tot.min.once.3,paste(DirCode,'/tables/Tab_media_models_theilsu_rank_cwstars_all_periods_at_least_once_third.csv',sep=''))
# write.csv(xtable(t.tot.min.once.3),paste(DirCode,'/tables/Tab_media_models_theilsu_rank_cwstars_all_periods_at_least_once_third',sep=''))
sel.mods=row.names(result)[lin]


# making descriptives tables for media data -------------------------------

df.media=df.unrevised[,c('ym',media.mods[,1])]
colnames(df.media)=c('ym',media.mods[,2])
df.media=df.media[grep('2001-01',df.media$ym):grep('2014-04',df.media$ym),]
df.media[,c('Monetary','Inflation')]=df.media[,c('Monetary','Inflation')]*100

# making figures of media indeces -----------------------------------------

# source(paste(DirCode,'/code/mti EXPERIMENT main code/media indicators figures.R',sep=''))

# processing matlab mcs results -------------------------------------------
Dlist=row.names(overview)[which(overview$D==1)]
Llist=row.names(overview)[which(overview$L==1)]
Dlnlist=row.names(overview)[which(overview$D.ln==1)]
D2lnlist=row.names(overview)[which(overview$D2ln==1)]

tL=data.frame(variable=Llist,grouping=overview[Llist,'group'],L=overview[Llist,'L'],D=0,D.ln=0,D2ln=0)
tD=data.frame(variable=paste('D',Dlist,sep=''),grouping=overview[Dlist,'group'],L=0,D=overview[Dlist,'D'],D.ln=0,D2ln=0)
tDln=data.frame(variable=paste('Dln',Dlnlist,sep=''),grouping=overview[Dlnlist,'group'],L=0,D=0,D.ln=overview[Dlnlist,'D.ln'],D2ln=0)
tD2ln=data.frame(variable=paste('DDln',D2lnlist,sep=''),grouping=overview[D2lnlist,'group'],L=0,D=0,D.ln=0,D2ln=overview[D2lnlist,'D2ln'])
grouping=rbind(tL,tD,tDln,tD2ln)
rm(Dlist,Llist,Dlnlist,D2lnlist,tL,tD,tDln,tD2ln)
grouping$variable=as.character(grouping$variable)
grouping=grouping[grouping$variable%in%sel.mods,]
# row.names(grouping)=grouping$variable


old.id=sapply(c(1:length(old.names)),function(x) which(grouping$variable%in%old.names[x]))

grouping$variable[old.id]=new.names



sel.mods[!sel.mods%in%grouping$variable]



fres=list()
sfeorr='sfe'
# # alternative 'sfer' for recession
for (h in 1:12){# h=1
        inc=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/includeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exc=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/excludeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exin=rbind(exc,inc)
        
        inc.ind=rep(1,nrow(exin))
        inc.ind[1:nrow(exc)]=0
        pval=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/pvalsSQ',sfeorr,h,'.csv',sep=''),header=F)
        
        mcs.h=data.frame(exin,pval,inc.ind)
        sfe.exp=sfe[lin,]
        
        mods.sfe=row.names(sfe.exp)
        if (sfeorr=='sfe'){
                result=rs[[h]] 
                result=result[mods.sfe,]
        }else{
                result=cr[[h]]
                result=result[mods.sfe,]
        }
        
        result[,c('model.id','pval','inc')]=NA
        result[mcs.h[,1],c('model.id','pval','inc')]=mcs.h  
        row.names(result)[which(row.names(result)%in%old.names)]=new.names
        fres[[h]]=result
        
}
nmod=nrow(result)
ttt=sapply(fres,function(x){
        t1=x[,c('theilsu')]
        t1=format(round(t1, 2), nsmall = 2)
        t1=as.character(t1)
        
        
        t1[x[2:nmod,'cw.p']<=0.05]=paste(t1[x[2:nmod,'cw.p']<=0.05],'star',sep='')
        t1[x[2:nmod,'cw.p']<=0.01]=paste(t1[x[2:nmod,'cw.p']<=0.01],'star',sep='')
        IN=(x[2:nmod,'inc']==1)
        IN=c(1,IN)==1
        t1[!IN]=''
        # t1=c('1.00',t1)
        t1[x[,'theilsu']<1]=paste('DA',t1[x[,'theilsu']<1],'DB',sep='')
        t2=data.frame(
                round(
                        rank(
                        x[,'rank.theilsu',drop=F]
                        )
                ,0)
                )
        
        t2[!IN,1]=''
        t2[IN,1]=paste('(',t2[IN,],')',sep='')
        t1[IN]=paste(t1[IN],t2[IN,1],sep=' ')
        t1=data.frame(t1)
#         t1=data.frame(t1,t2,stringsAsFactors = F)
#         
#         #         t2=round(x[,c('pval')],2)
#         #         t2=as.character(t2)
#         #         t2[x[,'inc']==1]=paste(t2[x[,'inc']==1],'+',sep='')
        #         t=data.frame(t1,t2)
        
        return(t1)
}
)
ttt=sapply(ttt,function(x)x)
row.names(ttt)=row.names(result)
names.tab=c('ar',media.mods[,2])
tab1=ttt[names.tab,]
row.names(tab1)[1]='AR'
# colnames(tab1)[seq(1,23,2)]=NA
colnames(tab1)=paste('h',1:12,sep=':')
tab1out=tab1[,c(1,3,6,12)]

allrank=sapply(fres,function(x)x$rank.theilsu)
row.names(allrank)=row.names(result)
media.mods.incD=c(media.mods[,2],paste('D',media.mods[,2],sep=''))
allrank=allrank[!row.names(allrank)%in%media.mods.incD,]
allrank=allrank[,c(1,3,6,12)]
almin=apply(allrank,2,which.min)
tttalt=ttt[row.names(allrank),]
alternative=tttalt[almin,c(1,3,6,12)]
t1=alternative
alternative[,]=''
diag(alternative)=diag(t1)

tab1out=rbind(alternative,tab1out[2:nrow(tab1out),])

out=stargazer(tab1out,summary=F
              ,title = "Theil's U (and ranking) for all models included in MCS, all periods"
              ,font.size ='scriptsize' 
              )

out=gsub('DA','\\\\textbf{',out)
out=gsub('DB','}',out)

writeLines(out,paste(DirCode,'/tables/allperiods.tex',sep=''))



# jetzt nur für die krise -------------------------------------------------

fres=list()
sfeorr='sfer'
# # alternative 'sfer' for recession
for (h in 1:12){# h=1
        inc=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/includeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exc=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/excludeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exin=rbind(exc,inc)
        
        inc.ind=rep(1,nrow(exin))
        inc.ind[1:nrow(exc)]=0
        pval=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/pvalsSQ',sfeorr,h,'.csv',sep=''),header=F)
        
        mcs.h=data.frame(exin,pval,inc.ind)
        sfe.exp=sfe[lin,]
        
        mods.sfe=row.names(sfe.exp)
        if (sfeorr=='sfe'){
                result=rs[[h]] 
                result=result[mods.sfe,]
        }else{
                result=cr[[h]]
                result=result[mods.sfe,]
        }
        
        result[,c('model.id','pval','inc')]=NA
        result[mcs.h[,1],c('model.id','pval','inc')]=mcs.h  
        row.names(result)[which(row.names(result)%in%old.names)]=new.names
        fres[[h]]=result
        
}
nmod=nrow(result)
ttt=sapply(fres,function(x){
        t1=x[,c('theilsu')]
        t1=format(round(t1, 2), nsmall = 2)
        t1=as.character(t1)
        
        
        t1[x[2:nmod,'cw.p']<=0.05]=paste(t1[x[2:nmod,'cw.p']<=0.05],'star',sep='')
        t1[x[2:nmod,'cw.p']<=0.01]=paste(t1[x[2:nmod,'cw.p']<=0.01],'star',sep='')
        IN=(x[2:nmod,'inc']==1)
        IN=c(1,IN)==1
        t1[!IN]=''
        # t1=c('1.00',t1)
        t1[x[,'theilsu']<1]=paste('DA',t1[x[,'theilsu']<1],'DB',sep='')
        t2=data.frame(
                round(
                        rank(
                                x[,'rank.theilsu',drop=F]
                        )
                        ,0)
        )
        
        t2[!IN,1]=''
        t2[IN,1]=paste('(',t2[IN,],')',sep='')
        t1[IN]=paste(t1[IN],t2[IN,1],sep=' ')
        t1=data.frame(t1)
        #         t1=data.frame(t1,t2,stringsAsFactors = F)
        #         
        #         #         t2=round(x[,c('pval')],2)
        #         #         t2=as.character(t2)
        #         #         t2[x[,'inc']==1]=paste(t2[x[,'inc']==1],'+',sep='')
        #         t=data.frame(t1,t2)
        
        return(t1)
}
)
ttt=sapply(ttt,function(x)x)
row.names(ttt)=row.names(result)
names.tab=c('ar',media.mods[,2])
tab1=ttt[names.tab,]
row.names(tab1)[1]='AR'
# colnames(tab1)[seq(1,23,2)]=NA
colnames(tab1)=paste('h',1:12,sep=':')
tab1out=tab1[,c(1,3,6,12)]

allrank=sapply(fres,function(x)x$rank.theilsu)
allrank=apply(allrank,2,rank)
row.names(allrank)=row.names(result)
media.mods.incD=c(media.mods[,2],paste('D',media.mods[,2],sep=''))
allrank=allrank[!row.names(allrank)%in%media.mods.incD,]
allrank=allrank[,c(1,3,6,12)]
almin=apply(allrank,2,which.min)
tttalt=ttt[row.names(allrank),]
alternative=tttalt[almin,c(1,3,6,12)]
t1=alternative
alternative[,]=''
diag(alternative)=diag(t1)

tab1out=rbind(alternative,tab1out[2:nrow(tab1out),])

out=stargazer(tab1out,summary=F
              ,title = "Theil's U (and ranking) for all models included in MCS, all periods"
              ,font.size ='scriptsize' 
)

out=gsub('DA','\\\\textbf{',out)
out=gsub('DB','}',out)

writeLines(out,paste(DirCode,'/tables/crisisperiods.tex',sep=''))
