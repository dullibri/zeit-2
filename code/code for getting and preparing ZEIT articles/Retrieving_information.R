#  ------------------------------------------------------------------------
# This file retrieves Meta-data from the first pages of plainhtml  --------
#  ------------------------------------------------------------------------
# Uses register containing the links, year, and issue of each article to crawl to
# the exisiting subdirectories containing plainhtml files.

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"


# Creating new columns ----------------------------------------------------

register$title_in_text=NA
register$date=NA
register$keywords=NA
# looping over the directories --------------------------------------------
for (i in 1:nrow(register)){
        plainhtml <- read.csv(paste(DirRawTexts,'/',register$year[i],'.',register$issue[i],'/',i,'.txt',sep=''))[-1]
        # plainhtml <-c(plainhtml)
        plainhtml=apply(plainhtml,2,as.character)
        plainhtml<-paste(plainhtml,sep="",collapse="")
        
        title_index=regexec(paste('<title>','(.*)',' DIE ZEIT Archiv',sep=''),plainhtml)
        register$title_in_text[i]=regmatches(plainhtml,title_index)[[1]][2]
        
        date_index=regexec(paste('date" content="','([0-9]{4}-[0-9]{2}-[0-9]{2})',sep=''),plainhtml)
        register$date[i]=regmatches(plainhtml,date_index)[[1]][2]
        
        keywords_index=regexec(paste('keywords" content="','(.*)','\"><(meta property)=\"og:site_name\"',sep=''),plainhtml)
        register$keywords[i]=regmatches(plainhtml,keywords_index)[[1]][2]
}



