DirCode='h:/Git/zeit-2'
DirCode='c:/users/dirk/documents/github/zeit-2'
# sourcing necessary scripts for estimation and forecast (for a description see "olsbmalag.Rmd")
auxcodedir=paste(DirCode,'/code/auxiliary code',sep='')
source(paste(auxcodedir,'/lag.exact.R',sep=''))
source(paste(auxcodedir,'/diff.R',sep=''))
source(paste(auxcodedir,'/chg.R',sep=''))
source(paste(auxcodedir,'/bmafo.R',sep=''))
source(paste(auxcodedir,'/olsbic3.R',sep=''))
source(paste(auxcodedir,'/olsbic1setgetting2.R',sep=''))
source(paste(auxcodedir,'/elanet.ind.R',sep=''))
# library('glmnet')
# library('elasticnet')
# library('glmulti')
# library('BMS')
# setting some values
# horizon=1 # inflation is published with on month lag, but in the following, the variables
# are only lagged.
zeitraus=0 # disregard zeit and rword indicator
target='CPI.EX' # core inflation
ic='aic' 
disregard=''# a variable that is a nearly perfect substitute for target.
max.lag=12 # maximum lag length to be considered 
max.obs=59 # maximum number of past observations to be considered (rolling estimation);
# setting negative window turns that of (recursive estimation)
# rolling window size will be: max.obs-max.lag-horizon, as lags need to be considered for
# estimation. 
start.2001.2=1 # data used starting in 2001:1 as media data are avaiable from here
retest.on=TRUE


# getting list of already existing variables
# old.file=paste(DirCode,'/results/rec_',ic,target,'_h',5,'.RData',sep='')
# load(old.file)
# aux=forecast.all[[1]]
# old.names=row.names(aux)
# old.names=old.names[-grep('ar',old.names)]
# rm(forecast.all)

# retest
# retest.raw=unlist(read.csv(paste(DirCode,'/data/retest.csv',sep='')
#                            ,header=F
#                            ,stringsAsFactors=F)
# )
# retest=c(retest.raw,
#          paste('D',retest.raw,sep='')
# )
# testing the variables in retest again


# loading realtime data sets and unrevised data 
load(paste(DirCode,'/data/realtime_sets_cutoffday_31.RData',sep=''))
df.unrevised=read.csv(paste(DirCode,'/data/data.csv'
                            ,sep='')
                      ,sep=','
                      ,na.strings='NA'
                      ,row.names=1
                      ,stringsAsFactors=FALSE
)

# dates as row.names, dates out of df
dat.col=(grep('ym',colnames(df.unrevised))+1):ncol(df.unrevised)

row.names(df.unrevised)=df.unrevised[,'ym']
df.unrevised=df.unrevised[,dat.col] 
rm(dat.col)
zeit.ind=grep('zeit',colnames(df.unrevised))
colnames(df.unrevised)=gsub('-','\\.',colnames(df.unrevised))


overview=read.csv(paste(DirCode,'/data/metadata.csv',sep=''),row.names=1)
row.names(overview)=gsub('-','\\.',row.names(overview))
# replacing "." with "-" and getting vector of names
names.nr=colnames(df.unrevised)
# names.nr[-grep('MT',names.nr)]=gsub('\\.','-',names.nr[-grep('MT',names.nr)])
# colnames(df.unrevised)=names.nr

# names.nr[grep('zeit',names.nr)]=gsub('-','\\.',names.nr[grep('zeit',names.nr)])

# getting overview of non-revised data
overview.nr=overview[names.nr,]

# Saving a complete version of unrevised data (below, some of them are lagged to take account of realtime publication lags) 
df.unrevised.compl=df.unrevised # df.unrevised=df.unrevised.compl

if (zeitraus==1){
  df.unrevised=df.unrevised[,-grep('zeit|rword',colnames(df.unrevised)),]
  overview=overview[-grep('zeit|rword',row.names(overview)),]
  overview.nr=overview.nr[-grep('zeit|rword',row.names(overview.nr)),]
  names.nr=names.nr[-grep('zeit|rword',names.nr)]
}



# getting vintage dates
# first: changing vint.names to correspond to other date format
vint.names=names(sets)
vint.names=gsub('-31','',vint.names)
tt=strsplit(vint.names,'-')
tt=t(sapply(tt,function(x)x))
for (i in 1:nrow(tt)){if (nchar(tt[i,2])==1){tt[i,2]=paste('0',tt[i,2],sep='')}}
vint.names=paste(tt[,1],tt[,2],sep='-')
names(sets)=vint.names

# all sets have the same codes in the same order?
test=t(sapply(sets,function(x) colnames(x)))
# replace code by names in sets
rt.code=test[1,]
# matching codes and names
overview.rt=overview[overview$code%in%rt.code,]
# which names correspond to the codes 
name.num=unlist(sapply(rt.code,function(x) which(x==overview.rt$code)))
rt.names=row.names(overview.rt)[name.num]

for (i in 1:length(sets)){colnames(sets[[i]])=rt.names}
#making a copy
sets.s=sets #sets=sets.s



# loop -----------------------------------------------------------

push.down=function(variable,set){
  # pushes all variables to the forecast origin so that data with publication
  # lag may be used nontheless for forecasting.
  aux=set[,variable]
  naux=length(aux)
  values=aux[is.na(aux)==F]
  nvalues=length(values)
  aux2=rep(NA,naux)
  aux2[(naux-nvalues+1):naux]=values
  return(aux2)
}


# defining the results to be extracted from estimated models 
extract.ar=c('fc','horizon','nobs','msr','p1','names')
extract=c('fc','horizon','nobs','msr','p1','p2','names')

# drop DISREGARD from overiew.
if (nchar(disregard)!=0){
  row.disregard=which(row.names(overview)==disregard)
  overview.d=overview[row.disregard,] # needed below
  overview=overview[-row.disregard,]
  if (overview.d$Source=='Buba RTDB'){
    row.disregard=which(row.names(overview.rt)==disregard)
    overview.rt=overview.rt[-row.disregard,]
  }else{
    row.disregard=which(row.names(overview.nr)==disregard)
    overview.nr=overview.nr[-row.disregard,]
  }
}
for (horizon in 3:8){# horizon=1
#   old.file=paste(DirCode,'/results/rec_',ic,target,'_h',horizon,'.RData',sep='')
#   load(old.file)
#   forecast.all.old=forecast.all
  forecast.all=vector('list',length(sets))
  elanet=list()
  for (vint.num in 1:length(sets)){# vint.num=1
    set.rt=sets[[vint.num]]
    set.last.date=names(sets[vint.num])
    set.lst.obs=grep(set.last.date,row.names(set.rt))
    set.rt=set.rt[1:set.lst.obs,]
    bridge_needed=sum(is.na(tail(set.rt[,target,drop=F])))
    horizon_ext=horizon+bridge_needed
    set.unrevised=df.unrevised[row.names(set.rt),]
    set=cbind(set.rt,set.unrevised)
    
    # drops DISREGARD variables 
    if (nchar(disregard)!=0){
      col.disregard=which(colnames(set)==disregard)
      set=set[,-col.disregard]
    }
    # DEFLATING some data 
    deflator=set$CPI/100
    deflated.var=row.names(overview)[which(overview$deflated==1)]
    deflated.var.new=paste(deflated.var[1:3],'R',sep='')
    deflated.var.new[4]=paste(deflated.var[4],'R',sep='')
    set[,deflated.var.new]=set[,deflated.var]*deflator
    
    # PUBLICATION LAG of unrevised data 
    # (no deflated data need to be lagged further)
    variables.tlag=row.names(overview.nr)[overview.nr$lag!=0]
    for (var.tlag in variables.tlag){
      set[,var.tlag]=lag.exact(set[,var.tlag,drop=F]
                               ,overview.nr[row.names(overview.nr)==var.tlag
                                            ,'lag'])
    }
    
    # DEPENDENT VARIABLE matrix, 
    # endogenous regressor and eliminating it in set.
    
    # if unclear, whether publication lag differs for realtime data, run:
    #         I=length(sets)
    #         lag.ip=matrix(NA,I,1)
    #         for (i in 1:I){
    #         lo=which(row.names(sets[[i]])==names(sets[i]))
    #         lag.ip[i]=sum(is.na(tail(sets[[i]]$'IP'[1:lo])))
    #         }
    y.raw=set[,target,drop=F]
    col.target=which(colnames(set)==target)
    set=set[,-col.target]
    rm(col.target)
    y=1200/horizon_ext*log(y.raw/lag.exact(y.raw,horizon_ext))
    yx=1200*log(y.raw/lag.exact(y.raw,1))
    colnames(yx)=paste(target,'x',sep='')
    
    # DIFFERENCES
    variables.d=row.names(overview)[!is.na(overview$D)]
    # target not in set anymore, therefore:
    variables.d=variables.d[!variables.d==target]
    variables.d.new.name=paste('D',variables.d,sep='')
    set[,variables.d.new.name]=diff(set[,variables.d],1,1)
    
    # LOG DIFFERENCES        
    variables.dln=row.names(overview)[!is.na(overview$D.ln)]
    # target not in set anymore, therefore:
    variables.dln=variables.dln[!variables.dln==target]
    variables.dln.new.name=paste('Dln',variables.dln,sep='')
    set[,variables.dln.new.name]=diff(log(set[,variables.dln]),1,1)
    
    # LOG 2.DIFFERENCES        
    variables.ddln=row.names(overview)[!is.na(overview$D2ln)]
    # target not in set anymore, therefore:
    variables.ddln=variables.ddln[!variables.ddln==target]
    variables.ddln.new.name=paste('DDln',variables.ddln,sep='')
    set[,variables.ddln.new.name]=diff(log(set[,variables.ddln]),2,1)
    
    # Eliminating SUPERFLUOUS LEVEL series.      
    variables.level=row.names(overview)[is.na(overview$L)]
    # target not in set anymore, therefore:
    variables.level=variables.level[!variables.level==target]
    
    variables.level.cols=which(colnames(set)%in%variables.level)
    set=set[,-variables.level.cols]
    
    # PUSHING DOWN all variables        
    variables=colnames(set)
    dates=row.names(set)
    
    y[,1]=push.down(target,y)
    yx[,1]=push.down(paste(target,'x',sep=''),yx)
    
    set=data.frame(sapply(variables,push.down,set))
    row.names(set)=dates
    
    if (start.2001.2==1){
      start=grep('2001-02',row.names(set))
      y=y[start:nrow(set),1,drop=F]
      yx=yx[start:nrow(set),1,drop=F]
      set=set[start:nrow(set),]  
    }
    
    
    
    # window of max.obs month back
    if (max.obs>0){
      y=y[(nrow(set)-max.obs+1):nrow(set),1,drop=F]
      yx=yx[(nrow(set)-max.obs+1):nrow(set),1,drop=F]
      set=set[(nrow(set)-max.obs+1):nrow(set),]
    }
    #                 elanet[[vint.num]]=elanet.ind(set,y,yx,horizon_ext,alpha=1)
    
    
    forecast=data.frame(matrix(NA,nrow=ncol(set)+1,length(extract)))
    colnames(forecast)=extract
    row.names(forecast)=c('ar',colnames(set))
    
    forecast['ar',extract.ar]=unlist(
      olsbic3(y,yx,horizon_ext,max.lag,ic)[extract]
    )
    p1=forecast['ar','p1']   
#     newnames=colnames(set)[!colnames(set)%in%old.names]
#     if (retest.on==F){
#       
#       set=set[,newnames]
#     }else{
#       estnames=c(newnames,retest)
#       set=set[,estnames]
#     }
    
    for (i in 1:ncol(set)){# i=1
      print(paste(vint.num,i,sep=':'))
      xx=cbind(yx,set[,i,drop=F])
      resu=unlist(
        olsbic1setgetting2(y,xx,horizon_ext,max.lag,p1=p1,ic)[extract]
      )
      if (length(resu)>0){
        forecast[colnames(set)[i],extract]=resu   
      }
      
    }# end model loop
#     forecast.old=forecast.all.old[[vint.num]]
#     if (retest.on==F){
#       forecast[row.names(forecast.old),]=forecast.old
#     }else{
#       forecast.redone=forecast
#       forecast[row.names(forecast.old),]=forecast.old
#       forecast[estnames,]=forecast.redone[estnames,]
#     }
    forecast.all[[vint.num]]=forecast
    
  }#end vintage loop
  
  lassm=matrix(NA,nrow=(ncol(set)+1),ncol=(length(sets)))
  row.names(lassm)=c(colnames(yx),colnames(set))
  #         for (i in 1:length(elanet)){#i=1
  #                 rlass=rank(elanet[[i]],ties.method = c("first"))
  #                 
  #                 names(rlass)=gsub(paste('L',horizon_ext,'$',sep=''),'',names(rlass))
  #                 row.names(lassm)[row.names(lassm)%in%names(rlass)]
  #                 lassm[names(rlass),i]=rlass  
  #                 
  #                 }
  
  #         write.csv(lassm,paste(DirCode,'/results/rec_elanet_ranking',ic,target,'_h',horizon_ext,'new.RData',sep=''))
  if (max.obs<0){meth='recursive'}else(meth=paste('rolling',max.obs,sep=''))
  save(forecast.all,file=paste(DirCode,'/results/',meth,ic,target,'_h',horizon,'.RData',sep=''))
  
  
}
