#  ------------------------------------------------------------------------
# This file converts plainhtml to plain text and stores the files ---------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
# DirCode='H:/git/zeit-2'
# load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"
DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"
DirRawTexts="C:/Users/Dirk/documents/github/zeit-2/data/goldstandard/texte"

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
        plainhtml_short=plainhtml[(txt_start+1):(txt_end-1)]
        
        txt_s<-convert_html_to_text(plainhtml_short)
        
        
        
        txt<-paste(txt_s,sep='',collapse='')
        txt<-gsub('\u0084|\u0093|\u0096',' ',txt)
        
        txt<-gsub('»|,|\\.|:|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|\\?|( (\t)* )',' ',txt)
        
        txt<-gsub('( ){2,}',' ',txt)# superfluos spaces eliminated
        
        return(txt)
}

library(SnowballC)
library(tm)
library(RCurl)
library(XML)

#  Getting registery------------------------------------------------------------------------




# subd="2008.49"       
listfiles=list.files(DirRawTexts) 

for (file in listfiles){
        write.csv(gettext(paste(DirRawTexts,'/',file,sep=''))
                  ,paste(DirRawTexts,'/','plaintxt-',file,sep=''),eol=''
                  ,row.names=F#,col.names=F
                  ,fileEncoding='UTF-8'
                  ,quote=F# only possible as there are no commas left
        )
        
}





