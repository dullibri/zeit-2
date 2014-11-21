
# Setting directory and settings -------------------------------------------------------
mainfile="H:/git/zeit-2"
# mainfile="C:/Users/Dirk/zeit"

setwd(mainfile)

# loading libraries -------------------------------------------------------
library(SnowballC)
library(tm)
library(RCurl)
library(XML)
library(kernlab)

# Getting texts -----------------------------------------------------------

fnextpages<-function(plainhtml,index){
  plainhtml_index=plainhtml[index]
  plainhtml_index_split=unlist(strsplit(plainhtml_index,'"'))
  links=plainhtml_index_split[grep('http',plainhtml_index_split)]
  unique(links) 
}



fgettext<-function(input,number){
        # input is the first http address of the text to be downloaded.
        # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 3 seiten
        plainhtml=readLines(input,encoding='UTF-8')
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
        write.csv(mittelteil,paste('H:/git/zeit-2/',number,'.txt',sep=''),fileEncoding='UTF-8')
        #   return()
}

for (i in 16671:nrow(register)){
  fgettext(input=as.character(register$link[i]),number=i)
}
rm(i)

