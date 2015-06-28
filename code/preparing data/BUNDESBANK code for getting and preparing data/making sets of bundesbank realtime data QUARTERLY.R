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
# wd='c:/users/dirk/documents/github/zeit-2'
library(zoo)
setwd(wd)
dir.rt=paste(wd,'/data',sep='')
keys=read.csv(paste(dir.rt,'/bundesbank_keys.csv',sep=''),stringsAsFactors=F)
# variable.file=list.files(paste(wd,'/data/bundesbankrealtime',sep=''))
# nvar=length(variable.file)

# loading data
 

variable=data.frame(matrix(c('Q.DE.Y.A.AG1.CA010.A.I','gdp'
                             ,'Q.DE.Y.A.CA1.BA100.A.I','cons'
                             ,'Q.DE.Y.A.CD1.BAA00.A.I','inv'
                             ,'Q.DE.Y.A.CX1.CA010.A.I','exp'
                             ,'Q.DE.Y.A.CM1.CA010.A.I','imp'
)
,ncol=2
,byrow=T
)
)
colnames(variable)=c('code','variable')
variable.file=variable[,1]
nvar=nrow(variable)
for (i in 1:nvar){
        var=read.csv(paste(wd,'/data/bundesbankrealtime/',variable.file[i],'.csv',sep=''),row.names=1)
        nobsx=nrow(var)
        # dropping meta data
        var=var[5:nobsx,]
        # transforming to numbers
        var=write.csv(var,paste(wd,'/deleteme.csv',sep=''))
        var=read.csv(paste(wd,'/deleteme.csv',sep=''),row.names=1)
        eval(parse(text=paste(variable[i,2],'=var',sep='')))
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
        var=eval(parse(text=as.character(variable[i,'variable'])))
        variable$fst_obs[i]=row.names(var)[1]
        variable$lst_obs[i]=row.names(var)[nrow(var)]
        variable$fst_vint[i]=colnames(var)[1]
        variable$lst_vint[i]=colnames(var)[ncol(var)]
        detail=unlist(strsplit(as.character(variable[i,1]),'\\.'))
        
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
var.used=variable[vint.late,]

reformat=function(x){
        aux=colnames(x)
        aux=gsub('X','',aux)
        aux=gsub('\\.[0-9]{2}$','',aux)
        aux=gsub('\\.','-',aux)
        
        
        return(aux)
}
gdp=gdp[-nrow(gdp),-ncol(gdp)]# gdp is published one month earlier than components
aux=list(exp,gdp,imp,cons)
tt=sapply(aux,reformat)
setnames=tt[,1]

nsets=ncol(gdp)
aux=1:nsets
setsq=list()
for (x in 1:nsets){# x=1
        setsq[[x]]=cbind(exp[,x,drop=F],gdp[,x],imp[,x],cons[,x])
        colnames(setsq[[x]])=c('exp','gdp','imp','cons')
        row.names(setsq[[x]])=gsub('-01','-1',row.names(setsq[[x]]))
        row.names(setsq[[x]])=gsub('-04','-2',row.names(setsq[[x]]))
        row.names(setsq[[x]])=gsub('-07','-3',row.names(setsq[[x]]))
        row.names(setsq[[x]])=gsub('-10','-4',row.names(setsq[[x]]))
}

names(setsq)=setnames
save(setsq,var.used, file = paste(wd,'/data/realtime_sets_quarterly.RData',sep=''))

