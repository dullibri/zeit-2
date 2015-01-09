#  ------------------------------------------------------------------------
# Starting values (do not comment) ---------------------------------------------------------
#  ------------------------------------------------------------------------


wd="I:/Personal_Folders/students/Jan/R"
setwd(wd)
dir.rt=paste(wd,'/data',sep='')

#  ------------------------------------------------------------------------
# Downloading and storing Data ---------------------------------------------------------
#  ------------------------------------------------------------------------

library("XLConnect")
library("xlsx")

dir.create(dir.rt)
# getting nodes
nodeaddress='http://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Real_Time_Data/realtime_zeitreihen_node.html'
nodehtml=readLines(nodeaddress)
nodes_id=grep('] <',nodehtml)
nodes=nodehtml[nodes_id]
# View(nodes)
nodes=sapply(strsplit(nodes,'>'),function(x)x[2])
nodes=gsub(' </a||]','',nodes)
nodes=t(sapply(strsplit(nodes,'\\['),function(x)x))
nodes=unique(nodes)


# searching the index page of data base -----------------------------------

startaddress='http://www.bundesbank.de/Navigation/EN/Statistics/Time_series_databases/Real_Time_Data/realtime_zeitreihen_node.html?openAll=true'
fileaddress=paste(startaddress,'files/',sep='')
starthtml=readLines(startaddress)
# write.csv(starthtml,'starthtml.txt') # for inspection of starthtml only 

# getting labels
# View(starthtml)
labels_id=grep('] <',starthtml)
labels=starthtml[labels_id]
# View(labels)
labels=sapply(strsplit(labels,'>'),function(x)x[2])
labels=gsub(' </a||]','',labels)
labels=t(sapply(strsplit(labels,'\\['),function(x)x))
labels=unique(labels)


labels[,1]=tolower(labels[,1])
labels=unique(labels)
labels=labels[-grep('at constant prices',labels[,1]),]
labels=labels[-grep('at current prices, flows',labels[,1]),]

labels.ordered=labels[order(labels[,2]),]

labels=data.frame(labels,stringsAsFactors=F)


variable_index=regexec("[AQM]\\.DE\\.[A-Z0-9\\.]{17}",starthtml)
variable_vector=regmatches(starthtml,variable_index)#[[1]][2]

for (i in length(variable_vector):1){
        if (length(variable_vector[[i]])==0){variable_vector[[i]]=NULL}
}
rm(i)
variable.names <- unlist(variable_vector)


#  ------------------------------------------------------------------------

# downloading files
# for (i in 421:length(variable.names)){
#   download.file(paste("http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=",variable.names[i],"&rtd_csvFormat=en&rtd_fileFormat=csv&mode=rtd&downloadType=matrix", sep=""), paste(dir.rt,'/',variable.names[i],".csv",sep=''),mode='wb')
# }

labels[grep(c('quarterly|annual|monthly'),labels[,1]),'group']=1
labels[grep(c('unadjusted figure|calendar and seasonally adjusted|unadjusted figure (neither seasonally nor calendar adjusted)
              |seasonally adjusted only'),labels[,1]),'group']=3
labels[grep(c('in absolute terms|in constant prices'),labels[,1]),'group']=NA
labels[grep(c('deviation in % of the long term average level|contribution to growth|
              in currency units using chain index for calculation|index'),labels[,1]),'group']=8


labels[which(labels[,2]=='P'&labels[,1]=='prices '),3]=8

nodes.ors=paste(nodes[1:3,1],collapse='|',sep='')
nodes.ors=gsub(' \\|','\\|',nodes.ors)     
nodes.ors=tolower(nodes.ors)
labels[grep(nodes.ors,labels[,1]),'group']=4
labels[labels[,2]=='R',3]=4

variables=sapply(variable.names,strsplit,'\\.')
variables=t(sapply(variables,function(x) x[1:8]))
# getting monthly variables
variables.mth=variables[variables[,1]=='M',]
labels.s=labels

aux5=match(labels[,2],variables[,5])
aux5[is.na(labels[,3])==F]=NA
labels[is.na(aux5)==F,3]=5

aux6=match(labels[,2],variables[,6])
aux6[is.na(labels[,3])==F]=NA
labels[is.na(aux6)==F,3]=6

aux7=match(labels[,2],variables[,7])
aux7[is.na(labels[,3])==F]=NA

labels[is.na(aux7)==F,3]=7
rm(aux5,aux6,aux7)
labels.ordered=labels[order(labels[,2]),]

topics=match(variables.mth[,5],labels[,2])
topics=labels[topics,1]
