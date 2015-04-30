
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
# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"

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
valueword=read.csv(paste(DirCode,'/data/SentiWS_v1.8c/valueword.csv',sep=''))

# Texte eines Jahres laden -----------------------------------------------
for (jj in 1990:2015){#jj=2015
        # list of subdirectories each year
        liste_jahr=listsubdirs[grep(as.character(jj),listsubdirs)]
        for (k in 1:length(liste_jahr)){#k=1
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
                
                Ergebnis=data.frame(matrix(NA,1,3))
                colnames(Ergebnis)=c('id','value','nword')
                
                for (i in 1:Narticle_issue){# i=1
                        text<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        tp=polarity(text,polarity.frame=pf,negators=negating)
                        sent=tp$all[c('polarity')]
                        
                        nwords<-tp$all[c('wc')]
                                               
                        Ergebnis[i,]=c(id=gsub('article-|\\.txt','',svFile[i])
                                       ,sent
                                       ,nword=nwords                                       
                        )
                        
                }
                write.csv(Ergebnis,paste(DirCode,'/data/zeit indikatoren/Ergebnis_qdap_negator_',jj,'_',k,'.csv',sep=''),row.names=F)
        }
        
        rm(i) 
        
}#Jahresschleife
