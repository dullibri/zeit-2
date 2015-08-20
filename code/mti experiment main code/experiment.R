
# file description --------------------------------------------------------

# This file implements a forecast experiment. It collects realtime data as available 
# at different moments in time. There are sets of revised data of the Bundesbank and
# unrevised data (Stock indexes and the like). The Bundesbank data are organized in sets
# containing vintages of 5 realtime variables each. "sets" is a list where the list names
# mark the month at which this time varying information was available. The number of sets
# is the maximum number of possible iterations.
# 
# The experiment is organized as a loop. Each loop a specific realtime-data vintage is 
# created, that is, Bundesbank data are merged with the unrevised data to form "set" 
# the complete data at that time. Publication lags are accounted for before merging them
# with the Bundesbank data. Then forecasts of different horizons are made for several models.
# First, an autoregressive model is fitted and forecasts are made, where p1 is the number
# of lags selected. Then using p1 lags of the endogenous, each model one additional exogenous
# variable is fitted with the lag number (p2) being estimated.
# 
# Note:
# If the endogenous comes with a lag, the forecast horizon is automatically extended. If, the lag
# is 2 and horizon is 1 then the actual forecast horizon is 3.
# "overview" contains the variables and the (possible different) transformations that are implemented.
# "data" contains the unrevised data, "sets" the revised data. All variables used must be contained in 
# "data" and "sets" AND in overview for the file to work.

DirCode='h:/Git/zeit-2'
# DirCode='c:/users/dirk/documents/github/zeit-2'

# sourcing necessary auxiliary scripts ----------------------------------------------
auxcodedir=paste(DirCode,'/code/auxiliary code',sep='')
source(paste(auxcodedir,'/lag.exact.R',sep=''))
source(paste(auxcodedir,'/diff.R',sep=''))
source(paste(auxcodedir,'/chg.R',sep=''))
source(paste(auxcodedir,'/push.down.R',sep=''))
source(paste(auxcodedir,'/bmafo.R',sep=''))
source(paste(auxcodedir,'/olsbic3.R',sep=''))
source(paste(auxcodedir,'/olsbic1setgetting2.R',sep=''))
source(paste(auxcodedir,'/elanet.ind.R',sep=''))

# setting some values -----------------------------------------------------
target='CPI.EX' # core inflation
ic='aic' # alternative: bic
vint.num.max=100 # up to which real-time set (=iteration) should the loop be implemented?
disregard=''# a variable that is a nearly perfect substitute for target.
max.lag=12 # maximum lag length to be considered 
max.obs=59 # maximum number of past observations to be considered (rolling estimation);
if (max.obs<0){meth='recursive'}else(meth=paste('rolling',max.obs,sep=''))
start.2001.2=1 # data used starting in 2001:1 as Mediatenor data are avaiable from here
# defining the results to be extracted from estimated models 
# extracts elements of the output list of the estimation files (labeled "ols")
# "fc" = forecast value, "horizon" = which horizon, "nobs" = number of observations,
# "msr" = insample mean squared error, "p1" = selected lag length of autoregressive component,
# "p2" = selected lag length of exogenous variable, "names" = variable names including 
# lagged variables used. 
extract.ar=c('fc','horizon','nobs','msr','p1','names')
extract=c('fc','horizon','nobs','msr','p1','p2','names')

# loading realtime data sets and unrevised data  --------------------------
load(paste(DirCode,'/data/realtime_sets_cutoffday_31.RData',sep=''))
df.unrevised=read.csv(paste(DirCode,'/data/data.csv'
                            ,sep='')
                      ,sep=','
                      ,na.strings='NA'
                      ,row.names=1
                      ,stringsAsFactors=FALSE
)

# df.unrevised: setting dates as rownames ---------------------------------
dat.col=(grep('ym',colnames(df.unrevised))+1):ncol(df.unrevised)
row.names(df.unrevised)=df.unrevised[,'ym']
df.unrevised=df.unrevised[,dat.col] 
rm(dat.col)
colnames(df.unrevised)=gsub('-','\\.',colnames(df.unrevised))

# reading overiew (info about variables and transformations) --------
overview=read.csv(paste(DirCode,'/data/metadata.csv',sep=''),row.names=1)
row.names(overview)=gsub('-','\\.',row.names(overview))
# replacing "." with "-" and getting vector of names
names.nr=colnames(df.unrevised)
# getting overview of non-revised data
overview.nr=overview[names.nr,]

# getting vintage dates from vintage data (=sets) -------------------------
# first: changing vint.names to correspond to other date format
vint.names=names(sets)
vint.names=gsub('-31','',vint.names)
tt=strsplit(vint.names,'-')
tt=t(sapply(tt,function(x)x))
for (i in 1:nrow(tt)){if (nchar(tt[i,2])==1){tt[i,2]=paste('0',tt[i,2],sep='')}}
vint.names=paste(tt[,1],tt[,2],sep='-')
names(sets)=vint.names
# all sets have the same Bundesbank-codes in the same order?
test=t(sapply(sets,function(x) colnames(x)))

# some renaming ensuring that Bundesbank codes and variable names  --------
# and variable names correspond

# replace code by names in sets
rt.code=test[1,]
# matching Bundesbank-codes and variable names using overview
overview.rt=overview[overview$code%in%rt.code,]
# which variable names correspond to the Bundesbank-codes 
name.num=unlist(sapply(rt.code,function(x) which(x==overview.rt$code)))
rt.names=row.names(overview.rt)[name.num]
for (i in 1:length(sets)){colnames(sets[[i]])=rt.names}
#making a copy
sets.s=sets #sets=sets.s

# loop -----------------------------------------------------------
for (horizon in 1:12){# horizon=2
        forecast.all=vector('list',length(sets))
        for (vint.num in 1:vint.num.max){# vint.num=1
                set.rt=sets[[vint.num]] # gets the respective realtime date for recursion
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
                variables.tlag=variables.tlag[is.na(variables.tlag)==F]
                for (var.tlag in variables.tlag){
                        set[,var.tlag]=lag.exact(set[,var.tlag,drop=F]
                                                 ,overview.nr[row.names(overview.nr)==var.tlag
                                                              ,'lag'])
                }
                
                # DEPENDENT VARIABLE matrix, 
                # endogenous regressor and eliminating it in set.
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
                
                forecast.all[[vint.num]]=forecast
                
        }#end vintage loop

        save(forecast.all,file=paste(DirCode,'/results/',meth,ic,target,'_h',horizon,'.RData',sep=''))
        
        
}
