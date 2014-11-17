
# getting complete register -----------------------------------------------

year=2013
input<-paste('http://www.zeit.de/',year,'/index/seite-3',sep='')
plainhtml=readLines(input,encoding='UTF-8')
unlink(input)

fnissue<-function(year,webpage){
  all_issue=matrix(NA,nrow=10,ncol=2)
for (i in 45:54){
  index=grep(paste('http://www.zeit.de/',year,'/',i,'/index',sep=''),plainhtml)
  all_issue[i-45+1,]=c(i,length(plainhtml[index]))
}
min(all_issue[all_issue[,2]==0,1])-1
}
Nissue=fnissue(year,plainhtml)

registry<-function(year,issue){
  issue_formated=as.character(issue)
  if (issue<10){
    issue_formated=paste(0,issue,sep='')
  }
  
  input <-paste('http://www.zeit.de/',year,'/',issue_formated,'/index',sep='')
  plainhtml=readLines(input,encoding='UTF-8')
  unlink(input)
  index=grep(paste('http://www.zeit.de/',year,'/',issue_formated,sep=''),plainhtml)
  register_raw=unique(plainhtml[index])
  register_raw=register_raw[-grep(input,register_raw)]

  plainhtml_index_split=unlist(strsplit(register_raw,'"'))
  links=plainhtml_index_split[grep('http',plainhtml_index_split)]
  titles=plainhtml_index_split[grep('title=',plainhtml_index_split)+1]
  descriptions=matrix(sapply(links,function(x){
    y=unlist(strsplit(x,'/'))
    y[length(y)]
  }))
#   sapply(list(links,titles,descriptions),duplicated)
  
  register=data.frame(link=links
                      ,title=titles
                      ,description=descriptions
                      ,year=year
                      ,issue=issue) 
  return(register)
}
for (issue in 1:Nissue){
  if (!'register'%in%ls()){
    register=registry(year,issue)
  }
  register=rbind(register
                 ,registry(year,issue))
}


# identifying mayor topics ----------------------------------------------


# Eleminating '-' of description entries to get topics --------------------
library(tm)
topics<-sapply(as.character(register$description),strsplit,'-')
topics<-sapply(topics,paste,collapse=' ')
topics=as.matrix(topics)

# topics <- register$description
VStopics <- VectorSource(topics)
Ctopics <- Corpus(VStopics, readerControl = list(reader = readPlain,
                                                 language="de"))


# Ctopics <- tm_map(Ctopics, function(x){strsplit(x,'-')})
DTMtopics=DocumentTermMatrix(Ctopics)
findFreqTerms(DTMtopics,200,Inf)
MDTMtopics<-as.matrix(DTMtopics)
Mtest=as.matrix(dtmtest)

