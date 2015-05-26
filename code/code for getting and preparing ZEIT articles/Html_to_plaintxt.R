#  ------------------------------------------------------------------------
# This file converts plainhtml to plain text and stores the files ---------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
# DirCode='H:/git/zeit-2'
# load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"
<<<<<<< HEAD
# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"
=======
DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"
DirRawTexts="E:/Zeit"
>>>>>>> 8b5aca6d3864275bc6cbf49f6b8cf7b133bb0701

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
<<<<<<< HEAD
        
        #         txt<-gsub('»|,|\\.|:|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|\\?|( (\t)* )',' ',txt)
        txt<-gsub('»|,|:|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|( (\t)* )',' ',txt)
        txt<-gsub('\\t|\\\\',' ',txt)
        
        txt<-gsub('( ){2,}',' ',txt)# superfluos spaces eliminated
        txt<-gsub(' \\.','\\.',txt)# superfluos spaces eliminated
        
=======
             
#         txt<-gsub('»|,|\\.|:|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|\\?|( (\t)* )',' ',txt)
        txt<-gsub('»|,|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|( (\t)* )',' ',txt)
        
        txt<-gsub('( ){2,}',' ',txt)# superfluos spaces eliminated
        txt<-gsub('  \\.','.',txt)# superfluos spaces eliminated

>>>>>>> 8b5aca6d3864275bc6cbf49f6b8cf7b133bb0701
        return(txt)
}

library(SnowballC)
library(tm)
library(RCurl)
library(XML)

#  Getting registery------------------------------------------------------------------------

listsubdirs=list.files(DirRawTexts)

for (subd in listsubdirs){
        # subd="2008.49"       
        listfiles=list.files(paste(DirRawTexts,'/',subd,sep='')) 
        if (length(grep('article',listfiles))!=0){
                listfiles=listfiles[-grep('article',listfiles)]       
        }
        if (length(grep('plaintxt',listfiles))!=0){
                listfiles=listfiles[-grep('plaintxt',listfiles)]       
        }
        if (length(grep('Ergebnis',listfiles))!=0){
                listfiles=listfiles[-grep('Ergebnis',listfiles)]       
        }
        for (file in listfiles){
                write.csv(gettext(paste(DirRawTexts,'/',subd,'/',file,sep=''))
                          ,paste(DirRawTexts,'/',subd,'/','plaintxt-',file,sep=''),eol=''
                          ,row.names=F#,col.names=F
                          ,fileEncoding='UTF-8'
                          ,quote=F# only possible as there are no commas left
                )
                
        }
}




