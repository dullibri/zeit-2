
# Setting directory and settings -------------------------------------------------------
mainfile="H:/git/zeit-2"
# mainfile="C:/Users/Dirk/zeit"
year=2014
issue=2

setwd(mainfile)

# loading libraries -------------------------------------------------------
library(SnowballC)
library(tm)
library(RCurl)
library(XML)
library(kernlab)

# Getting links -----------------------------------------------------------

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
# Getting texts -----------------------------------------------------------

fnextpages<-function(plainhtml,index){
  plainhtml_index=plainhtml[index]
  plainhtml_index_split=unlist(strsplit(plainhtml_index,'"'))
  links=plainhtml_index_split[grep('http',plainhtml_index_split)]
  unique(links) 
}
convert_html_to_text <- function(html) {
  # extracted from: convert_html_to_text <- function(html) {
  doc <- htmlParse(html, asText = TRUE,encoding='UTF-8')
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  return(text)
}


fgettext<-function(input,year,issue,title,outfile){
  # input is the first http address of the text to be downloaded.
  # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 3 seiten
  input=as.character(input)
  plainhtml=readLines(input,encoding='UTF-8')
  unlink(input)
  
  mittelteil=plainhtml[(grep('articleheader',plainhtml)+1):(grep('articlefooter',plainhtml)-1)]
#   mittelteil=readHTML(mittelteil,mittelteil.html,mittelteil.txt)
  mittelteil<-convert_html_to_text(mittelteil)
  index=grep(paste(input,'/seite-',sep=''),plainhtml)
  if (length(index)!=0){
    weitere_seiten=fnextpages(plainhtml,index)
    seiten_zahl=length(weitere_seiten)+1
    for (i in 2:seiten_zahl){
      plainhtml=readLines(weitere_seiten[i-1],encoding='UTF-8')
      unlink(input)
      zusaetzlicher_text=plainhtml[(grep('articleheader',plainhtml)+1):(grep('articlefooter',plainhtml)-1)]
      zusaetzlicher_text<-convert_html_to_text(zusaetzlicher_text)
      mittelteil=c(mittelteil,zusaetzlicher_text)
    }
  }
  mittelteil=mittelteil[mittelteil!="\n"]
  mittelteil=mittelteil[mittelteil!="\n"]
  mittelteil=gsub('\n','',mittelteil,)
  mittelteil=paste(mittelteil,collapse='')
  write.csv(mittelteil,paste(outfile,'/',title,'.txt',sep=''),fileEncoding='UTF-8')
#   return()
}


# running the code --------------------------------------------------------

register=registry(year,issue)
narticle=nrow(register)
tfile=paste(mainfile,'/',year,'-',issue,sep='')
if (sum(dir()==paste(year,'-',issue,sep=''))==0){
  dir.create(file.path(tfile))
}
# for (i in 1:1){
for (i in 1:narticle){
  fgettext(input=register$link[i],year=year,issue=issue,title=register$description[i],outfile=tfile)
}
write.csv(register,paste(tfile,'/register.csv',sep=''))
rm(i)


# Creating Corpus ---------------------------------------------------------
Dir<-DirSource(directory=tfile,encoding='UTF-8',recursive = TRUE)
zeitcorp <- Corpus(x=Dir, 
                        readerControl = list(reader = readPlain,
                                             language="de"))
rohtext <- Corpus(x=Dir, 
                   readerControl = list(reader = readPlain,
                                        language="de"))

zeitcorp <- tm_map(zeitcorp, stripWhitespace)
zeitcorp <- tm_map(zeitcorp, removeNumbers)
zeitcorp <- tm_map(zeitcorp, removePunctuation)
zeitcorp <- tm_map(zeitcorp, tolower)
zeitcorp <- tm_map(zeitcorp, function(x){removeWords(x,
                                                   c("dass",stopwords("german")))})
zeitcorp <- tm_map(zeitcorp,function(x){stemDocument(x,language = "german")})
Tdmzeit <- TermDocumentMatrix(zeitcorp)
Mtdmzeit<- as.matrix(Tdmzeit)

# save document term matrix and raw text corpus ---------------------------

save(Tdmzeit,rohtext, file = paste(tfile,'/dtm-rawtext-',year,'-',issue,".RData",sep=''))


# Reduktion auf die Wörter, die mehr als 600 mal vorkommen 
kurz<-Mtdmzeit[which(rownames(Mtdmzeit)%in%findFreqTerms(Tdmzeit,100,Inf)),]



# identify topics automatically using kernlab -----------------------------
test.class<-DMetaData(zeitcorp)