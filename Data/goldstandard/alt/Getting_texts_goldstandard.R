
# Setting directory and settings -------------------------------------------------------
mainfile="H:/git/zeit-2"

goldstandardwww <- read.table("C:/Users/Dirk/GitHub/zeit-2/Data/goldstandard/alt/goldstandardwww.csv", dec=",", quote="\"")
register=data.frame(link=unique(goldstandardwww))
colnames(register)='link'
mainfile="C:/Users/Dirk/github/zeit-2/data/goldstandard/alt"
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

# registry<-function(year,issue){
#   issue_formated=c(paste(rep(0,9),1:9,sep=''),paste(10:12,sep=''))
#   input <-paste('http://www.zeit.de/',year,'/',issue_formated[issue],'/index',sep='')
#   plainhtml=readLines(input,encoding='UTF-8')
#   unlink(input)
#   index=grep(paste('http://www.zeit.de/',year,'/',issue_formated[issue],sep=''),plainhtml)
#   register_raw=unique(plainhtml[index])
#   register_raw=register_raw[-grep(input,register_raw)]
#   t1=register_raw[1]
#   
#   plainhtml_index_split=unlist(strsplit(register_raw,'"'))
#   links=plainhtml_index_split[grep('http',plainhtml_index_split)]
#   titles=plainhtml_index_split[grep('title=',plainhtml_index_split)+1]
#   descriptions=matrix(sapply(links,function(x){
#     y=unlist(strsplit(x,'/'))
#     y[length(y)]
#   }))
#   
#   register=data.frame(link=unique(links)
#                       ,title=titles
#                       ,description=descriptions
#                       ,year=year
#                       ,issue=issue) 
#   return(register)
# }
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

# fgettext<-function(input,year,issue,title,outfile){
#   # input is the first http address of the text to be downloaded.
#   # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 3 seiten
#   input=as.character(input)
#   plainhtml=readLines(input,encoding='UTF-8')
#   unlink(input)
#   
#   mittelteil=plainhtml[(grep('articleheader',plainhtml)+1):(grep('articlefooter',plainhtml)-1)]
# #   mittelteil=readHTML(mittelteil,mittelteil.html,mittelteil.txt)
#   mittelteil<-convert_html_to_text(mittelteil)
#   index=grep(paste(input,'/seite-',sep=''),plainhtml)
#   if (length(index)!=0){
#     weitere_seiten=fnextpages(plainhtml,index)
#     seiten_zahl=length(weitere_seiten)+1
#     for (i in 2:seiten_zahl){
#       plainhtml=readLines(weitere_seiten[i-1],encoding='UTF-8')
#       unlink(input)
#       zusaetzlicher_text=plainhtml[(grep('articleheader',plainhtml)+1):(grep('articlefooter',plainhtml)-1)]
#       zusaetzlicher_text<-convert_html_to_text(zusaetzlicher_text)
#       mittelteil=c(mittelteil,zusaetzlicher_text)
#     }
#   }
#   mittelteil=mittelteil[mittelteil!="\n"]
#   mittelteil=mittelteil[mittelteil!="\n"]
#   mittelteil=gsub('\n','',mittelteil,)
#   mittelteil=paste(mittelteil,collapse='')
#   write.csv(mittelteil,paste(outfile,'/',title,'.txt',sep=''),fileEncoding='UTF-8')
# #   return()
# }


fgettext<-function(input,number){
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
        write.csv(mittelteil,paste(mainfile,'/',number,'.txt',sep=''),fileEncoding='UTF-8')
        #   return()
}


# getting the texts --------------------------------------------------------


for (i in 1:nrow(register)){
  fgettext(input=register$link[i],number=i)
}
rm(i)

# Registry analysis -------------------------------------------------------

test=interaction(register$year,register$issue)
test1=tapply(register$title,test,length)
testdate=character(nrow(register))
for (i in 1:nrow(register)){
        testdate[i]=paste(register$year,register$issue,' ')
}             
date=as.Date(,
             format = "%m/%d/%y")             
             
input<-'h:/zeit/1990.1/1.txt'

gettext<-function(input){
        # Uses "convert_html_to_text.R"
        # input is the Dir and the name of the .txt file in html
        # example: input<-'h:/zeit/1990.1/1.txt'
        # Returns: plain text
        plainhtml<-readLines(input,encoding='UTF-8')
        text_index=regexec(paste('zol_inarticletools','(.*)','articlefooter',sep=''),plainhtml)
        text=regmatches(plainhtml,text_index)[[1]][2]
        
        txt_start=grep('zol_inarticletools',plainhtml)
        txt_end=grep('articlefooter',plainhtml)
        txt_raw=plainhtml[(txt_start+1):(txt_end-1)]
        
        txt<-convert_html_to_text(txt_raw)
        txt<-txt[-grep('"\n',txt)] 
}




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