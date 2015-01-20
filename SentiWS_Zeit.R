
# Beschreibung ------------------------------------------------------------

# dieser code liest zeit-artikel ein und bewertet sie anhand von SentiWS.
# 1. die anzahl der positiven und negativen Worten wird mit den Bewertungszahlen gewichtet und
#    aufsummiert
# 2. die anzahl der positiven und negativen worte wird berechnet
# 3. die bewertung auf positiv und negativ aufgesplittet wird angegeben.


# Letzte Modifikation -------------------------------------------------------------------
# 2015-01-17, 17:00 komplet revidiert, tm package raus.
# 2015-01-15, 20:45
# 2014-12-01, 17:08
# 2014-11-25, 17:14
# 2014-06-20, 22:22


# Setting directories for storing files -------------------------------------------------------
# DirRawTexts="H:/Zeit" # text files are stored here
DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"

# DirCode='H:/git/zeit-2' # main directory
DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"
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
        ind = valueword[, 1] %in% text.split
        valdf = valueword[ind, , drop = F]
        valdf$h = sapply(valueword[ind, 1], function(x) sum(text.split %in% 
                                                                    x))
        nwords = length(text.split)
        return(list(valdf,nwords))
}

# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)

#  SentiWS: ------------------------------------------------------------------------
#       getting the list of positive and negative words and their values -----------------------------------------------------------------
#       valueword (lowercase) VALUEWORD (uppercase), created with valueword.R
valueword=read.csv(paste(DirCode,'/valueword.csv',sep=''))

# Texte eines Jahres laden -----------------------------------------------
for (jj in 2014:2015){#jj=2015
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
                
                Ergebnis=data.frame(matrix(NA,1,6))
                colnames(Ergebnis)=c('id','npword','nnword','nword','pvalue','nvalue')
                
                for (i in 1:Narticle_issue){# i=1
                        text<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        sent<-sentiment(text,valueword)
                        nwords<-sent[[2]]
                        sent.df<-sent[[1]]
                        pos.id=which(sent.df[,'wert']>0)
                        
                        Ergebnis[i,]=c(id=gsub('article-|\\.txt','',svFile[i])
                                ,npword=sum(sent.df[pos.id,'h'])
                                ,nnword=sum(sent.df[-pos.id,'h'])
                                ,nword=nwords
                                ,pvalue=sum(sent.df[pos.id,'wert']*sent.df[pos.id,'h'])
                                ,nvalue=sum(sent.df[-pos.id,'wert']*sent.df[-pos.id,'h'])
                        )
                        
                }
                write.csv(Ergebnis,paste(sFolderTexte,'Ergebnis.csv',sep=''),row.names=F)
        }
                
                rm(i) 
                
}#Jahresschleife
