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
keys=read.csv(paste(dir.rt,'/bundesbank_keys.csv',sep=''),stringsAsFactors=F)
variable.file=list.files(paste(wd,'/data/bundesbankrealtime',sep=''))
nvar=length(variable.file)
variable=gsub('.csv','',variable.file)
variable=data.frame(variable,stringsAsFactors=F)
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



variable$fst_obs=NA
variable$lst_obs=NA
variable$fst_vint=NA
variable$lst_vint=NA
variable[,paste('key',1:8)]=NA
# columnnumber of keys
keycol=grep('key',colnames(variable))
nvar=nrow(variable)
for (i in 1:nvar){
        var=eval(parse(text=variable[i,'variable']))
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
                # what is the key
                key=variable[j,keycol[i]]
                # look up what it means
                pos=grep(key,keylist[,2])
                label=keylist[pos,1]
                variable[j,labelcol[i]]=label
        }
}
tu=strsplit(variable$fst_vint,'\\.')
tu=sapply(tu,function(x) x)
tu=gsub('X','',tu)
tu=matrix(as.integer(tu),nrow=3)
for (i in 1:nrow(variable)){
        if (tu[1,i]<1000){
                variable$fstvint.year[i]=as.integer(t(tu[3,i]))   
                variable$fstvint.month[i]=as.integer(t(tu[2,i])) 
        }
        if (tu[1,i]>1000){
                variable$fstvint.year[i]=as.integer(t(tu[1,i]))   
                variable$fstvint.month[i]=as.integer(t(tu[2,i])) 
        }
        
}
# dropping those variables that have vintages that start later than 2005
vint.late=variable$fstvint.year<=2005
variable.sel=variable[vint.late,]

# downsizing of data
variable.sel=variable.sel[variable.sel$'key 1'=='M',]
variable.sel=variable.sel[-grep('current prices',variable.sel$'label7'),]
variable.sel=variable.sel[-grep('neither seasonally nor calendar adjusted',variable.sel$'label3'),]
variable.sel=variable.sel[-grep('domestic|abroad',variable.sel$'label5'),]

## Real economy
# orders
sel=grep('order',variable.sel$'label5')
var.order=variable.sel[sel,]
var.order=var.order[grep('manufacturing|consumer|capital',var.order$'label6'),]
var.used=var.order

# prices
sel=grep('price',variable.sel$'label4')
var.cpi=variable.sel[sel,]
var.cpi=var.cpi[grep('all categories|total, excluding energy',var.cpi$'label6'),]
var.used=rbind(var.used,var.cpi)
# hours
sel=grep('hours worked by employed persons',variable.sel$'label5')
var.hours=variable.sel[sel,]
var.hours=var.hours[grep('in absolute terms',var.hours$'label8'),]
var.used=rbind(var.used,var.hours)
# hours
sel=grep('employed persons',variable.sel$'label5')
var.employ=variable.sel[sel,]
var.employ=var.employ[grep('overall economy',var.employ$'label6'),]
var.used=rbind(var.used,employ)
# wage
var.wage=variable[grep('wage',variable$'label5'),]
var.wage=var.wage[grep('negotiated wages and salaries',var.wage$'label5'),]
var.wage=var.wage[grep('overall',var.wage$'label6'),]
var.wage=var.wage[grep('negotiated wages and salaries per hour',var.wage$'label5'),]
var.used=rbind(var.used,var.wage)

# intermediate goods
var.interm=variable.sel[grep('intermediate goods',variable.sel$'label6'),]
var.interm=var.interm[grep('production',var.interm$'label5'),] 
var.used=rbind(var.used,var.interm)

# industrial production
var.ip=variable.sel[-sel,]
var.ip=var.ip[grep('including construction|excluding construction',var.ip$'label6'),]
var.used=rbind(var.used,var.ip)

var.used$label=c('ORD','ORD-I','ORD-C','CPI-EX','CPI','WHOUR','TARIF','IP-VORL','IP-CONST','IP')
var.used$transformation=c('Dln','Dln','Dln','D ln, DD ln','D ln, DD ln','L, D','D ln, DD ln','Dln','Dln','Dln')
# variables used
nvar=nrow(var.used)
eval(parse(text=paste('var=',var.used$variable[1],sep='')))


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
tt=sapply(var.used$variable,function(x) vint.survey(x,cutoffday)$vintage)
# create a list of vintages per variable used 
vintage.employ=cbind(dates[,c(1,2)],tt)
start=vintage.employ$year==2005&vintage.employ$month==12
start=which(start)
end=vintage.employ$year==2015&vintage.employ$month==2
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
var.used[var.used$label=='WHOUR','L']=1
var.used[var.used$label=='WHOUR','D']=1
var.used[var.used$label%in%c('ORD','ORD-C','ORD-I','CPI','CPI-EX','IP','IP-VORL','IP-CONST','TARIF'),'Dln']=1
var.used[var.used$label%in%c('CPI','CPI-EX','TARIF'),'D2ln']=1
var.used$Source='Buba RTDB'
colnames(var.used)[which(colnames(var.used)=='variable')]='code'
var.used$name[which(var.used$label=='ORD')]='Manufacturing orders'
var.used$name[which(var.used$label=='ORD-C')]='Manufacturing orders - consumer goods'
var.used$name[which(var.used$label=='ORD-I')]='Manufacturing orders - capital goods'
var.used$name[which(var.used$label=='CPI')]='CPI'
var.used$name[which(var.used$label=='CPI-EX')]='Core CPI'
var.used$name[which(var.used$label=='TARIF')]='Negotiated wage and salary level'
var.used$name[which(var.used$label=='IP')]='Industrial production'
var.used$name[which(var.used$label=='IP-CONST')]='Industrial production excluding construction'
var.used$name[which(var.used$label=='IP-VORL')]='Intermediate goods production'
var.used$name[which(var.used$label=='WHOUR')]='Hours worked'

var.used$id[which(var.used$label=='ORD')]=83
var.used$id[which(var.used$label=='ORD-C')]=84
var.used$id[which(var.used$label=='ORD-I')]=85
var.used$id[which(var.used$label=='CPI')]=78
var.used$id[which(var.used$label=='CPI-EX')]=79
var.used$id[which(var.used$label=='TARIF')]=80
var.used$id[which(var.used$label=='IP')]=0
var.used$id[which(var.used$label=='IP-CONST')]=-1
var.used$id[which(var.used$label=='IP-VORL')]=82
var.used$id[which(var.used$label=='WHOUR')]=90

var.used$lag=NA
var.used$lag[which(var.used$label=='ORD')]=1
var.used$lag[which(var.used$label=='ORD-C')]=1
var.used$lag[which(var.used$label=='ORD-I')]=1
var.used$lag[which(var.used$label=='CPI')]=0
var.used$lag[which(var.used$label=='CPI-EX')]=1
var.used$lag[which(var.used$label=='TARIF')]=1
var.used$lag[which(var.used$label=='IP')]=1
var.used$lag[which(var.used$label=='IP-CONST')]=1
var.used$lag[which(var.used$label=='IP-VORL')]=1
var.used$lag[which(var.used$label=='WHOUR')]=1

var.used.s=var.used

var.used=var.used[,c("name","id","L","D","Dln","D2ln",'lag',"code")]  
row.names(var.used)=var.used.s$label     
var.used$Source='Buba RTDB'
write.csv(var.used,'h:/Git/zeit-2/data/bubaRTDmeta.csv')
save.image("C:/Users/dulbricht/Desktop/t.RData")
save(sets,var.used, file = paste(wd,'/data/realtime_sets_cutoffday_',cutoffday,".RData",sep=''))
