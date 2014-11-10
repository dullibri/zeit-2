
# getting complete register -----------------------------------------------
fnextpages<-function(plainhtml,index){
  plainhtml_index=plainhtml[index]
  plainhtml_index_split=unlist(strsplit(plainhtml_index,'"'))
  links=plainhtml_index_split[grep('http',plainhtml_index_split)]
  unique(links) 
}
year=2013
input<-paste('http://www.zeit.de/',year,'/index/seite-3',sep='')
# input<-'http://www.zeit.de/2013/index/seite-3'
plainhtml=readLines(input,encoding='UTF-8')
unlink(input)

index=grep(paste('http://www.zeit.de/',year,sep=''),plainhtml)
plainhtml[index]
page=fnextpages(plainhtml,index)
index_page=page[grep('index',page)]
write.csv(plainhtml,'test.txt')
registry<-function(year,issue){
  issue_formated=c(paste(rep(0,9),1:9,sep=''),paste(10:12,sep=''))
  input <-paste('http://www.zeit.de/',year,'/',issue_formated[issue],'/index',sep='')
  plainhtml=readLines(input,encoding='UTF-8')
  unlink(input)
  index=grep(paste('http://www.zeit.de/',year,'/',issue_formated[issue],sep=''),plainhtml)
  register_raw=unique(plainhtml[index])
  register_raw=register_raw[-grep(input,register_raw)]
  t1=register_raw[1]
  
  plainhtml_index_split=unlist(strsplit(register_raw,'"'))
  links=plainhtml_index_split[grep('http',plainhtml_index_split)]
  titles=plainhtml_index_split[grep('title=',plainhtml_index_split)+1]
  descriptions=matrix(sapply(links,function(x){
    y=unlist(strsplit(x,'/'))
    y[length(y)]
  }))
  
  register=data.frame(link=unique(links)
                      ,title=titles
                      ,description=descriptions
                      ,year=year
                      ,issue=issue) 
  return(register)
}
register=registry(year,issue)


# deriving topics from title ----------------------------------------------

test=paste(register$title,collapse=' ')
test=VectorSource(register$title)
test=Corpus(test, readerControl = list(reader = readPlain,
                                       language="de"))

test <- tm_map(test, tolower)
test <- tm_map(test, function(x){removeWords(x,
                                             c("dass",stopwords("german")))})
test <- tm_map(test,function(x){stemDocument(x,language = "german")})
dtmtest=DocumentTermMatrix(test)
findFreqTerms(dtmtest,2,Inf)
Mtest=as.matrix(dtmtest)
kurz<-Mtest[which(rownames(Mtest)%in%findFreqTerms(dtmtest,4,Inf)),]