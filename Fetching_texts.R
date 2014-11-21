#  ------------------------------------------------------------------------
# This file uses the links in register to download and store text  --------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"

# Creating subdirectories where texts are stored ----------------------------------------------------------
register=cbind(register,yearissue=interaction(register$year,register$issue))
yi=unique(register$yearissue)
Nobs_yi=length(yi)
for (i in 1:Nobs_yi){
        dir.create(file.path(DirRawTexts, as.character(yi[i])))
}

# loading libraries -------------------------------------------------------
library(SnowballC)
library(tm)
library(RCurl)
library(XML)

# Getting texts -----------------------------------------------------------
fgettext<-function(input,number,year,issue){
        # downloads and stores html texts in the respective subdirectories.
        # input is the first http address of the text to be downloaded.
        # number is the id of the respective text in register, 
        # year and issue are taken from the register, as well.
        # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 5 seiten
        # input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien/seite-6'# 3 seiten
        # input <-'http://www.zeit.de/1985/01/kontinuitaet-mit-hindernissen' # 3 seiten alt, ohne übersicht
        # input <-'http://www.zeit.de/2000/08/200008.1._leiter_pol_.xml'
        plainhtml=readLines(input,encoding='UTF-8')
        write.csv(plainhtml,paste(DirRawTexts,'/',year,'.',issue,'/',number,'.txt',sep=''),fileEncoding='UTF-8')
        Np=1
        # Testing for more pages (Np>1) and retrieving ----------------------------        
        if (length(grep('data-pn-end',plainhtml)+1)!=0){
                #                 plainhtml=apply(plainhtml,2,as.character)
                plainhtml<-paste(plainhtml,sep="",collapse="")
                Np_index=regexec('data-pn-end="([0-9]|[0-9][0-9])"',plainhtml)
                Np=regmatches(plainhtml,Np_index)[[1]][2]
                for (a in 1:Np){
                        write.csv(readLines(paste(input,'/seite-',a,sep=''),encoding='UTF-8')
                                  ,paste(DirRawTexts,'/',year,'.',issue,'/',number,'-',a,'.txt',sep='')
                        )
                }
        }
        return(Np)
}

for (i in 1:nrow(register)){#
  register$Npages=fgettext(input=as.character(register$link[i])
           ,number=i
           ,year=register$year[i]
           ,issue=register$issue[i]
           )
}
rm(i)



