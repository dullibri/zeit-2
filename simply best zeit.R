# which of the simple zeit indicators is best when compared to ifo; "simple" refers to indicators, 
# that are not based on transformations such as variance or kurtosis, as this might hinder inter-
# pretability of the results.

DirRawTexts="H:/Zeit" # text files are stored here
DirCode='H:/git/zeit-2' # main directory
setwd(DirCode)

# getting list of variables
variables=list.files(paste(DirCode,'/results',sep=''))
variables=variables[-grep('border',variables)]

# creating a result data.frame
test=read.csv(paste(DirCode,'/results/',variables[1],'/rmse_hit_',variables[1],'_',1,'.csv',sep=''),row.names=1)
zeit.pref=grep('zeit',row.names(test))
zeit.disr=grep('kur|var',row.names(test)[zeit.pref])
zeit.pref=zeit.pref[-zeit.disr]
result=data.frame(model=row.names(test)[zeit.pref])

# looping over variables and horizons
horizon=1
variable=1
for (variable in variables)
for (horizon in c(1,3,6,12)){
test=read.csv(paste(DirCode,'/results/',variable,'/rmse_hit_',variable,'_',horizon,'.csv',sep=''),row.names=1)

ifo=grep('R[123]',row.names(test))
ifo.min=min(ifo)

zeit.pref=grep('zeit',row.names(test))
zeit.disr=grep('kur|var',row.names(test)[zeit.pref])
zeit.pref=zeit.pref[-zeit.disr]

good.mod=row.names(test)[zeit.pref[which(zeit.pref<ifo.min)]]
good.mod=c(good.mod)


# result[result$model%in%good.mod,'CA_1']=rep(1,length(good.mod))
# eval(parse(text=paste('result$',variables[1],'_',hor,'[',good.mod',]',sep='')))=1
eval(parse(text=paste('result[result$model%in%good.mod,\'',variable,'_',horizon,'\']=rep(1,length(good.mod))',sep='')))
}

best=data.frame(hits=rowSums(result[,2:ncol(result)],na.rm=T))
row.names(best)=result$model
best=best[order(best$hits,decreasing=T),,drop=F]
head(best)

