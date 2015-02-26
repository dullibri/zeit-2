# Setting directory and settings -------------------------------------------------------
mainfile="H:/git/zeit-2"

goldstandardwww <- read.table("C:/Users/Dirk/documents/GitHub/zeit-2/Data/goldstandard/alt/goldstandardwww.csv", dec=",", quote="\"")
register=data.frame(link=unique(goldstandardwww))
colnames(register)='link'
mainfile="C:/Users/Dirk/documents/github/zeit-2/data/goldstandard/alt"

#  ------------------------------------------------------------------------
# This file uses the links in register to download and store text  --------
#  ------------------------------------------------------------------------



# Setting directory for storing files -------------------------------------------------------
DirRawTexts="C:/Users/Dirk/documents/github/zeit-2/data/goldstandard/texte"



# loading libraries -------------------------------------------------------
library(SnowballC)
library(tm)
library(RCurl)
library(XML)

# Getting texts -----------------------------------------------------------
fgettext<-function(input,number){
        # downloads and stores html texts in the respective subdirectories.
        # input is the first http address of the text to be downloaded.
        # number is the id of the respective text in register, 
        # year and issue are taken from the register, as well.
        # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 5 seiten
        # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien/seite-6'# 3 seiten
        # input <-'http://www.zeit.de/1985/01/kontinuitaet-mit-hindernissen' # 3 seiten alt, ohne übersicht
        # input <-'http://www.zeit.de/2000/08/200008.1._leiter_pol_.xml'
        plainhtml=readLines(input,encoding='UTF-8')
        write.csv(plainhtml,paste(DirRawTexts,'/',number,'.txt',sep=''),fileEncoding='UTF-8')
        Np=1
        # Testing for more pages (Np>1) and retrieving ----------------------------        
        if (length(grep('data-pn-end',plainhtml)+1)!=0){
                #                 plainhtml=apply(plainhtml,2,as.character)
                plainhtml<-paste(plainhtml,sep="",collapse="")
                Np_index=regexec('data-pn-end="([0-9]|[0-9][0-9]|[0-9][0-9][0-9])"',plainhtml)
                Np=regmatches(plainhtml,Np_index)[[1]][2]
                if (as.numeric(Np)>20){
                        return(Np)     
                }else{
                        for (a in 1:Np){
                                write.csv(readLines(paste(input,'/seite-',a,sep=''),encoding='UTF-8')
                                          ,paste(DirRawTexts,'/',number,'-',a,'.txt',sep='')
                                          ,row.names=F)
                        }
                }
        }
        return(Np)
}

for (i in 1:nrow(register)){#
        
        
        fgettext(input=as.character(register$link[i])
                 ,number=i
                 
        )
}
rm(i)


