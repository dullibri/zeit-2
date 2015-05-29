
# Beschreibung ------------------------------------------------------------

# dieser code liest zeit-artikel ein und bewertet sie anhand von SentiWS.
# 1. die anzahl der positiven und negativen Worten wird mit den Bewertungszahlen gewichtet und
#    aufsummiert
# 2. die anzahl der positiven und negativen worte wird berechnet
# 3. die bewertung auf positiv und negativ aufgesplittet wird angegeben.


# Letzte Modifikation -------------------------------------------------------------------
# 2015-04-29, 16:40 qdap als alternative einführen. Ausgabeordner auf Resultate legen.
# 2015-01-17, 17:00 komplet revidiert, tm package raus.
# 2015-01-15, 20:45
# 2014-12-01, 17:08
# 2014-11-25, 17:14
# 2014-06-20, 22:22

# ZIEL: QDAP HIER EINZUFÜGEN
# Setting directories for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit" # text files are stored here
# DirRawTexts="E:/Zeit" # text files are stored here
# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"
library('qdap')
DirCode='H:/git/zeit-2' # main directory
# DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"
setwd(DirCode)

# Load register created by 'Getting_register.R' ---------------------------
# load(paste(DirCode,"/register.RData",sep=''))

sentiment<-function (text, valueword){
        # returns the word, value, stem, form and frequency of each sentiment word in text
        # in the data.frame valdf. And, it returns the total number of words in text
        # as integer nword.
        if (length(text) == 2 & text[1] == ",x") {
                text = text[2]
        }
        text.split = sapply(strsplit(text, " "), function(x) x)
        grep('\\.',text)
        ind = valueword[, 1] %in% text.split
        #         ind.rev=text.split%in% valueword[, 1]
        # ind.neg=text.split%in% negating
        valdf = valueword[ind, , drop = F]
        valdf$h = sapply(valueword[ind, 1], function(x) sum(text.split %in% 
                                                                    x))
        nwords = length(text.split)
        return(list(valdf,nwords))
}

valueword=read.csv(paste(DirCode,'/data/SentiWS_v1.8c/valueword.csv',sep=''))


# extracting information from sentiment -----------------------------------
sent2=sapply(sent2,function(x)x)
extracts=function(sent2){
        tab=sent2[[1]]
        if (sum(tab$wert)==0){return(rep(-999,9))}
        negind=tab$wert<0
        posind=tab$wert>0
        nneg=sum(tab$h[negind])
        npos=sum(tab$h[posind])
        negv=sum(tab$h[negind]*tab$wert[negind])
        posv=sum(tab$h[posind]*tab$wert[posind])
        # middle values desregarded -----------------------------------------------
        
        posextind=posind&tab$wert>0.1
        if (sum(posextind)){posvext=sum(tab$wert[posextind])}else{posvext=0}
        
        negextind=negind&tab$wert<(0.1*-1)
        if (sum(negextind)){negvext=sum(tab$wert[negextind])}else{negvext=0}
        
        valuext=posvext+negvext
        
        nwords=sent2[[2]]
        value=posv+negv
        res=data.frame(nneg,npos,negv,posv,nwords,value,posvext,negvext,valuext)
        return(res)
}



# preparing valueword for qdap --------------------------------------------

pos=as.character(valueword['wert'>0,'wort',drop=T])
# pos=tolower(pos)
pos.w=valueword['wert'>0,'wert']
neg=as.character(valueword[valueword$wert<0,'wort',drop=T])
# neg=tolower(neg)
neg.w=valueword[valueword$wert<0,'wert']
pf=sentiment_frame(pos,neg,pos.w,neg.w)

negating=read.csv(paste(DirCode,'/data/sentistrength_de/negators.csv',sep=''),header=F,stringsAsFactors=F)
negating=unlist(negating)
# testing
# tp=polarity(text,polarity.frame=pf,negators=negating)
# tpa=tp$all[c('wc','polarity')]

# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)

#  SentiWS: ------------------------------------------------------------------------
#       getting the list of positive and negative words and their values -----------------------------------------------------------------
#       valueword (lowercase) VALUEWORD (uppercase), created with valueword.R


# Texte eines Jahres laden -----------------------------------------------
for (jj in 1990:2014){#jj={#:2015 jj=1990 jj=2014
        # list of subdirectories each year
        liste_jahr=listsubdirs[grep(as.character(jj),listsubdirs)]
        for (k in 1:length(liste_jahr)){#k=50
                sFolderTexte=paste(DirRawTexts,'/',liste_jahr[k],'/',sep='')
                print(sFolderTexte)
                # getting list and number of articles
                svFile=list.files(sFolderTexte)
                svFile=svFile[grep('article',svFile)]
                Narticle_issue=length(svFile)
                
                if (Narticle_issue==1){
                        print(paste('ACHTUNG: ',sFolderTexte,' hat nur einen Text'))        
                        next
                }
                # initialize Results data.frame
                
                Ergebnis=data.frame(matrix(NA,1,12))
                colnames(Ergebnis)=c('id','qdap_value','qdap_nword'  
                                     ,'sent_nneg','sent_npos','sent_negv'
                                     ,'sent_posv','sent_nwords','sent_val'
                                     ,'posextind','negextind','valuext'
                )
                
                for (i in 1:Narticle_issue){# i=1
#                                                 text<-readLines(paste(sFolderTexte,svFile[i],sep=''), ok=F,encoding="UTF-8")#, header=T,stringsAsFactors =F)
                                                text=read.csv(paste(sFolderTexte,svFile[i],sep=''),stringsAsFactors=F,header=F)
                                                #                         if (nrow(text)>1){
                                                #                                 text=text[2,1]
                                                #                         }
                                                chrtest=sapply(text,function(x) is.character(x))
                                                text=paste(text,collapse=' ',sep='')
                                                tp=polarity(text,polarity.frame=pf,negators=negating)
                                                sent=tp$all[c('polarity')]
                                                
                                                nwords<-tp$all[c('wc')]
                                                # sentiment ---------------------------------------------------------------
                                                sent2=sentiment(text,valueword)
                                                sentres=extracts(sent2)
                                                Ergebnis[i,1:3]=c(id=gsub('article-|\\.txt','',svFile[i])
                                                               ,sent
                                                               ,nword=nwords 
                                                               
                                                )
                        
                                                Ergebnis[i,4:12]=unlist(sentres)
                        
#                                                 Ergebnis[i,'id']=gsub('article-|\\.txt','',svFile[i])
                        
                }
                write.csv(Ergebnis,paste(DirCode,'/data/zeit indikatoren/Ergebnis_',liste_jahr[k],'.csv',sep=''),row.names=F)
        }
        
        rm(i) 
        
}#Jahresschleife


