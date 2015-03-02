wd="h:/git/zeit-2"
library(zoo)
setwd(wd)
dir.rt=paste(wd,'/data',sep='')
keys=read.csv(paste(dir.rt,'/bundesbank_keys.csv',sep=''),stringsAsFactors=F)
variable.file=list.files(paste(wd,'/data/bundesbankrealtime',sep=''))
nvar=length(variable.file)

# loading data
for (i in 1:nvar){
        var=read.csv(paste(wd,'/data/bundesbankrealtime/',variable.file[i],sep=''),row.names=1)
        nobsx=nrow(var)
        # dropping meta data
        var=var[5:nobsx,]
        # transforming to numbers
        var=write.csv(var,paste(wd,'/deleteme.csv',sep=''))
        var=read.csv(paste(wd,'/deleteme.csv',sep=''),row.names=1)
        eval(parse(text=paste(variable[i,1],'=var',sep='')))
}

variable=gsub('.csv','',variable.file)
variable=data.frame(variable,stringsAsFactors=F)
variable$fst_obs=NA
variable$lst_obs=NA
variable$fst_vint=NA
variable$lst_vint=NA
variable[,paste('key',1:8)]=NA
keycol=grep('key',colnames(variable))
for (i in 1:nvar){
        var=eval(parse(text=variable[i,1]))
        variable$fst_obs[i]=row.names(var)[1]
        variable$lst_obs[i]=row.names(var)[nrow(var)]
        variable$fst_vint[i]=colnames(var)[1]
        variable$lst_vint[i]=colnames(var)[ncol(var)]
        detail=unlist(strsplit(variable[i,1],'\\.'))
        
        variable[i,keycol]=detail
}

# setting labels
variable[,paste('label',1:8,sep='')]=NA
labelcol=grep('label',colnames(variable))

for (i in 1:8){
        keylist=keys[keys$position==i,c(1,2)]
        for (j in 1:nvar){
                key=variable[j,keycol[i]]
                pos=grep(key,keylist[,2])
                label=keylist[pos,1]
                variable[j,labelcol[i]]=label
        }
}

# downsizing of data
variable.sel=variable[variable$'key 1'=='M',]

## Real economy
# orders
order.sel=grep('order',variable.sel$'label5')
var.order=variable.sel[order.sel,]
var.order=var.order[-grep('in current prices, flows',var.order$'label7'),]
var.order=var.order[-grep('unadjusted',var.order$'label3'),]
var.order=var.order[-grep('domestic|abroad',var.order$'label5'),]
# manufacturing is available until vintage 2005 something, industry is better
# industry and constructions starts only at about this vintage
var.order=var.order[grep('intermediate|consumer|capital',var.order$'label6'),]

# industrial production
var.ip=variable.sel[-order.sel,]
var.ip=var.ip[grep('calendar and seasonally adjusted',var.ip$'label3'),]
var.ip=var.ip[-grep('in current prices, flows',var.ip$'label7'),]
var.ip=var.ip[grep('including construction|excluding construction',var.ip$'label6'),]

# variables used
var.used=rbind(var.ip,var.order)
nvar=nrow(var.used)
eval(parse(text=paste('var=',var.used$variable[1],sep='')))
# number of vintages is equal across variables
nvint=ncol(var)


# settings for vintage publication survey
dates=data.frame(year=rep(1995:2015,each=12))
dates$month=rep(1:12,length(1995:2015))
dates$number=NA
dates$vintage=NA
dates$before=NA # latest before cut-off "day"
dates$last=NA # last obs of month
dates[,paste(1:31)]=NA

vint.survey=function(variable.name,day=15){
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
                        before=pubdays[(pubdays-day)<=0]
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
tt=sapply(var.used$variable,function(x) vint.survey(x)$vintage)
# create a list of vintages per variable used 
vintage.employ=cbind(dates[,c(1,2,3)],tt)



t2=as.Date(vintu[[2]],'%Y.%m.%d')
t3=as.Date(vintu[[3]],'%Y.%m.%d')
t11=zoo(1:240,t1)
t21=zoo(1:240,t2)
t31=zoo(1:240,t3)
tt=cbind(t11,t21,t31)
t1=zoo(1,vintu[1])
t3=zoo(1,vintu[3])
t3=rbind(t1,t2)
View(test)
# making sets
sets=vector(nvint,mode='list')
for (vint in 1:nvint){
        sets[[vint]]=data.frame(matrix(NA,nrow=nrow(var),ncol=nvar))
        for (var.i in 1:nvar){
                text=paste('sets[[',vint,']][,',var.i,']=',var.used$variable[var.i],'[,',vint,']',sep='')
                eval(parse(text=text))
        }
}
M.DE.Y.I.IP1.AA020.C.I[, 1]
sets[[1]]
