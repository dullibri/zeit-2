# this file collects the csv files containing the Bundesbank realtime data base.
# it loads the dictionary of keys of bundesbank
# it creates an overview of all variables and connects it to the keys and labels
# in "variable"
# it selects the variables employed in "should we trust in leading indicators..." by
# drechsel and scheufele 2010 in var.used
# based on this, it enumerates all the vintages that have been available until the
# "cutoffday" of a month for each month. using vintage.survey (function) resulting in
# "vintage.employed.
# this is used to construct sets of vintages of the variables used at a given point
# in time. They are named after their cutoffday, month and year.

wd="h:/git/zeit-2"
library(zoo)
setwd(wd)
dir.rt=paste(wd,'/data',sep='')
metadata=read.csv(paste(wd,'/data/metadata.csv',sep=''))
metadata=metadata[grep('Buba RTDB',metadata$Source),]
metadata=metadata[metadata$used==1,]
# keys=read.csv(paste(dir.rt,'/bundesbank_keys.csv',sep=''),stringsAsFactors=F)
variable.file=metadata$code
nvar=nrow(metadata)

# loading data


for (i in 1:nvar){
        var=read.csv(paste(wd,'/data/bundesbankrealtime/',variable.file[i],'.csv',sep=''),row.names=1)
        nobsx=nrow(var)
        # dropping meta data
        var=var[5:nobsx,]
        # transforming to numbers
        var=write.csv(var,paste(wd,'/deleteme.csv',sep=''))
        var=read.csv(paste(wd,'/deleteme.csv',sep=''),row.names=1)
        eval(parse(text=paste(metadata$code[i],'=var',sep='')))
}



metadata$fst_obs=NA
metadata$lst_obs=NA
metadata$fst_vint=NA
metadata$lst_vint=NA
metadata[,paste('key',1:8)]=NA
# columnnumber of keys
keycol=grep('key',colnames(metadata))
nvar=nrow(metadata)
for (i in 1:nvar){
        var=eval( parse( text=as.character( metadata[i,'code']) ) )
        metadata$fst_obs[i]=row.names(var)[1]
        metadata$lst_obs[i]=row.names(var)[nrow(var)]
        metadata$fst_vint[i]=colnames(var)[1]
        metadata$lst_vint[i]=colnames(var)[ncol(var)]
        detail=unlist(strsplit(as.character(metadata$code[i]),'\\.'))
        
        metadata[i,keycol]=detail
}

#
tu=strsplit(metadata$fst_vint,'\\.')
tu=sapply(tu,function(x) x)
tu=gsub('X','',tu)
tu=matrix(as.integer(tu),nrow=3)
for (i in 1:nrow(metadata)){
        if (tu[1,i]<1000){
                metadata$fstvint.year[i]=as.integer(t(tu[3,i]))   
                metadata$fstvint.month[i]=as.integer(t(tu[2,i])) 
        }
        if (tu[1,i]>1000){
                metadata$fstvint.year[i]=as.integer(t(tu[1,i]))   
                metadata$fstvint.month[i]=as.integer(t(tu[2,i])) 
        }
        
}
# dropping those variables that have vintages that start later than 2005
vint.late=metadata$fstvint.year<=2005
metadata.sel=metadata[vint.late,]



# 
# settings for vintage publication survey
dates=data.frame(year=rep(1995:2015,each=12))
dates$month=rep(1:12,length(1995:2015))
dates$number=NA
dates$vintage=NA
dates$before=NA # latest before cut-off "day"
dates$last=NA # last obs of month
dates[,paste(1:31)]=NA
cutoffday=31
vint.survey=function(variable.name,cutoffday){
#         variable.name=var.used$variable[11]
        # day: if 15 all the closest publication date including 15 will be returned
        
        # creates a dataframe containing year and month and each publication each month
        text=paste('var=',variable.name,sep='')
        eval(parse(text=text))
        t1=as.Date(colnames(var),'X%Y.%m.%d')
        tt=data.frame(date=t1)
        tt$month=as.numeric(format(t1,'%m'))
        tt$day=as.numeric(format(t1,'%d'))
        tt$year=as.numeric(format(t1,'%Y'))
        
        ntt=nrow(tt)
        for (i in 1:ntt){
                dates[tt$year[i]==dates$year&tt$month[i]==dates$month,paste(tt$day[i])]=as.character(tt$date[i])
        }
        ndates=nrow(dates)
        for (i in 1:ndates){
                dates$number[i]=sum(is.na(dates[i,paste(1:31)])==F)
                pubdays=which(is.na(dates[i,paste(1:31)])==F)
                if (length(pubdays)>0){
                        before=pubdays[(pubdays-cutoffday)<=0]
                        last=max(pubdays)
                        if (length(before)>=1){
                                dates$before[i]=dates[i,paste(min(before))]  
                                dates$vintage[i]=dates$before[i]
                        }else{
                                dates$before[i]=NA
                        }
                        dates$last[i]=dates[i,paste(last)]
                        
                        
                }
                if (i>1){
                        if(is.na(dates$before[i])==T&is.na(dates$last[i-1])==F){
                                dates$vintage[i]=dates$last[i-1]
                                dates$last[i]=dates$last[i-1]
                        }      
                }
                
                
        }
        dates$vintage=paste('X',dates$vintage,sep='')
        dates$vintage=gsub('-','\\.',dates$vintage)
        return(dates)
}
tt=sapply(metadata$code,function(x) vint.survey(x,cutoffday)$vintage)
colnames(tt)=metadata$code
# create a list of vintages per variable used 
vintage.employ=cbind(dates[,c(1,2)],tt)
start=vintage.employ$year==2005&vintage.employ$month==12
start=which(start)
end=vintage.employ$year==2015&vintage.employ$month==5
end=which(end)
vintage.employ=vintage.employ[start:end,]
nvint=nrow(vintage.employ)
nvar=ncol(vintage.employ)-2
dates=data.frame(year=rep(1990:2015,each=12))
mth=c(paste(0,1:9,sep=''),paste(10:12,sep=''))
m=rep(mth,length(1990:2015))
y=as.character(rep(1990:2015,each=12))
ym=paste(y,m,sep='-')


specimen=data.frame(matrix(NA,nrow=length(ym),ncol=nvar))
row.names(specimen)=ym
colnames(specimen)=colnames(vintage.employ)[3:(nvar+2)]

specimen_s=specimen
# making sets
sets=list()
for (vint in 1:nvint){
        
        for (var.i in colnames(specimen_s)){
                vintage=as.character(vintage.employ[vint,var.i])
                text=paste('var=',var.i,sep='')
                eval(parse(text=text))
                rnames=row.names(var)
                specimen[rnames,var.i]=var[,vintage]
        }
        # cutoff is the date <= the observations are included
        cutoff=paste(vintage.employ$year[vint],vintage.employ$month[vint],cutoffday,sep='-')
        sets[[cutoff]]=specimen
        specimen=specimen_s
}

save(sets, file = paste(wd,'/data/realtime_sets_cutoffday_',cutoffday,".RData",sep=''))
