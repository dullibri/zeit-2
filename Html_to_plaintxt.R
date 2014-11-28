#  ------------------------------------------------------------------------
# This file converts plainhtml to plain text and stores the files ---------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep='')

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"

convert_html_to_text <- function(html) {
        # extracted from: convert_html_to_text <- function(html) {
        doc <- htmlParse(html, asText = TRUE,encoding='UTF-8')
        text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
        return(text)
}
gettext<-function(input){
        # Uses "convert_html_to_text.R"
        # input is the Dir and the name of the .txt file in html
        # example: input<-'h:/zeit/1990.1/1.txt'
        # Returns: plain text
        plainhtml<-readLines(input,encoding='UTF-8')
#         text_index=regexec(paste('zol_inarticletools','(.*)','articlefooter',sep=''),plainhtml)
#         text=regmatches(plainhtml,text_index)[[1]][2]
#         
        txt_start=grep('end: header',plainhtml)
        txt_end=grep('articlefooter',plainhtml)
#         txt_raw=plainhtml[(txt_start+1):(txt_end-1)]
        
        txt<-convert_html_to_text(plainhtml[(txt_start+1):(txt_end-1)])
        txt<-txt[-grep('\"([0-9]){3}\",\"',txt)]
#         txt<-txt[-grep('"\n',txt)] 
}

library(SnowballC)
library(tm)
library(RCurl)
library(XML)

#  Getting registery------------------------------------------------------------------------

listsubdirs=list.files(DirRawTexts)

for (subd in listsubdirs[1082:1281]){
        listfiles=list.files(paste(DirRawTexts,'/',subd,sep='')) 
        if (length(grep('article',listfiles))!=0){
                listfiles=listfiles[-grep('article',listfiles)]       
        }
        if (length(grep('plaintxt',listfiles))!=0){
                listfiles=listfiles[-grep('plaintxt',listfiles)]       
        }
        for (file in listfiles){
                write.csv(as.matrix(gettext(paste(DirRawTexts
                                                  ,'/',subd
                                                  ,'/',file
                                                  ,sep=''
                )
                )
                )
                ,paste(DirRawTexts
                       ,'/',subd
                       ,'/','plaintxt-'
                       ,file
                       ,sep='')
                ,eol=''
                ,row.names=F
                ,col.names=F
                ,fileEncoding='UTF-8'
                )
        }
}
