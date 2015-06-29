#  ------------------------------------------------------------------------
# Starting values (do not comment) ---------------------------------------------------------
#  ------------------------------------------------------------------------


wd="h:/git/zeit-2"
setwd(wd)
dir.rt=paste(wd,'/data',sep='')

#  ------------------------------------------------------------------------
# Downloading and storing Data ---------------------------------------------------------
#  ------------------------------------------------------------------------

library("XLConnect")
library("xlsx")

# dir.create(dir.rt)

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

metadata=read.csv(paste(wd,'/data/metadata.csv',sep=''))
metadata=metadata[grep('Buba RTDB',metadata$Source),]
metadata=metadata[metadata$used==1,]
#  ------------------------------------------------------------------------
# missing!! variable.names[637]='M.DE.S.P.PC1.PC200'
# downloading files
for (i in 1:nrow(metadata)){
  download.file(paste("http://www.bundesbank.de/cae/servlet/StatisticDownload?tsId=",metadata$code[i],"&rtd_csvFormat=en&rtd_fileFormat=csv&mode=rtd&downloadType=matrix", sep=""), paste(dir.rt,'/BundesbankRealtime/',metadata$code[i],".csv",sep=''),mode='wb')
}


