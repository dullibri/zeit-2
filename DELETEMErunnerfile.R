#  ------------------------------------------------------------------------
# This file converts plainhtml to plain text and stores the files ---------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
# DirCode='H:/git/zeit-2'
# load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit2"

# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"

# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"
# DirRawTexts="E:/Zeit"


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
        
        
        #         txt<-gsub('»|,|\\.|:|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|\\?|( (\t)* )',' ',txt)
        txt<-gsub('»|,|:|<|>|\\n|"|[0-9]{1,20}|-|«|\\)|\\(|( (\t)* )',' ',txt)
        txt<-gsub('\\t|\\\\',' ',txt)
        
        txt<-gsub('( ){2,}',' ',txt)# superfluos spaces eliminated
        txt<-gsub(' \\.','\\.',txt)# superfluos spaces eliminated
        
        
        
        #         txt<-gsub('»|,|\\.|:|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|\\?|( (\t)* )',' ',txt)
        txt<-gsub('»|,|<|>|\\n|"|[0-9]{1,20}|;|-|«|\\)|\\(|( (\t)* )',' ',txt)
        
        txt<-gsub('( ){2,}',' ',txt)# superfluos spaces eliminated
        txt<-gsub('  \\.','.',txt)# superfluos spaces eliminated
        # t=data.frame(person='dirk',text=txt)
        # tt=sentSplit(t,'text')
        
        return(txt)
}

library(SnowballC)
library(tm)
library(RCurl)
library(XML)

#  Getting registery------------------------------------------------------------------------

listsubdirs=list.files(DirRawTexts)
nsubdirs=length(listsubdirs)

for (i in 155:nsubdirs){
        subd=listsubdirs[i]
        print(subd)
        # subd="1990.1"       
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
        for (file in listfiles){#file=listfiles[1]
                write.csv(gettext(paste(DirRawTexts,'/',subd,'/',file,sep=''))
                          ,paste(DirRawTexts,'/',subd,'/','plaintxt-',file,sep=''),eol=''
                          ,row.names=F#,col.names=F
                          ,fileEncoding='UTF-8'
                          ,quote=F# only possible as there are no commas left
                )
                
        }
}


#  ------------------------------------------------------------------------
# This file puts the different plaintexts of each article together --------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
# DirCode='H:/git/zeit-2'
# load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit2"
# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"
# DirRawTexts="E:/Zeit"



# Getting subdirectories --------------------------------------------------

listsubdirs=list.files(DirRawTexts)
nsubdirs=length(listsubdirs)

for (i in 1:nsubdirs){
        subd=listsubdirs[i]
        print(subd)
        # subd='1990.1'
        # subd='2004.6'
        # List of documents in plaintext ------------------------------------------
        listfiles=dir(paste(DirRawTexts,'/',subd,sep=''))
        listplaintexts=listfiles[grep('plaintxt',listfiles)]
        ids=gsub('plaintxt','',listplaintexts)
        ids=gsub('.txt','',ids)
        ids=gsub('^-','',ids)
        
        
        
        # Texts with multiple pages -----------------------------------------------
        mtext=ids[grep('-',ids)]
        
        mtext=strsplit(mtext,'-')
        mtext=t(sapply(mtext,function(x)x))
        #         mtext=t(sapply(mtext,function(x){as.numeric(x[1:2])}))
        if (!length(mtext)==0){
                idsm=unique(mtext[,1])
                idsm=sort(idsm)
        }
        # Files with just one page ------------------------------------------------
        if (!length(mtext)==0){stext=as.numeric(ids[-grep('-',ids)])}else{
                stext=ids
        }
        if (!length(mtext)==0){stext=stext[(stext%in%idsm)==0]} # wenn es texte mit mehreren seiten gibt
        stext=sort(stext)
        # onepager: copy plaintexts into articles  --------------------------------
        for (i in 1:length(stext)){
                file.copy(paste(DirRawTexts,'/',subd,'/','plaintxt-',stext[i],'.txt',sep=''),
                          paste(DirRawTexts,'/',subd,'/','article-',stext[i],'.txt',sep='')
                          ,overwrite=T)
        }
        rm(i)
        
        
        # m-pager: aggregate and save them as articles -------------------------------------------------
        if (!length(mtext)==0){
                for (i in 1:length(idsm)){# i=1
                        nidsm=nrow(mtext[mtext[,1]==idsm[i],]) # number of pages
                        article=character(nidsm) 
                        for (page in 1:nidsm){#page=1
                                
                                article[page]=readLines(paste(DirRawTexts,'/',subd,'/','plaintxt-',idsm[i],'-',page,'.txt',sep=''),encoding='UTF-8')
                                article[page]=gsub('^x ','',article[page])
                        }
                        article=paste(article,sep="",collapse="")
                        #                         if (nchar(as.character(register$title[idsm[i]]))<120){
                        #                                 article=gsub(register$title[idsm[i]],'',article)
                        #                         }
                        write.csv(article,
                                  paste(DirRawTexts,'/',subd,'/','article-',idsm[i],'.txt',sep=''),
                                  ,fileEncoding='UTF-8'
                                  ,quote=F                                
                        )
                }
        }
}

# sapply(mtext,strsplit,'-')







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
DirRawTexts="H:/Zeit2" # text files are stored here
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
# sent2=sapply(sent2,function(x)x)
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

negating=read.csv(paste(DirCode,'/data/qdap/negators_ready.csv',sep=''),header=F,stringsAsFactors=F)[,2]
negating=negating[-1]

amplifiers=read.csv(paste(DirCode,'/data/qdap/amplifiers_ready.csv',sep=''),header=F,stringsAsFactors=F)[,2]
amplifiers=amplifiers[-1]

deamplifiers=read.csv(paste(DirCode,'/data/qdap/deamplifiers_ready.csv',sep=''),header=F,stringsAsFactors=F)[,2]
deamplifiers=deamplifiers[-1]
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
                colnames(Ergebnis)=c('id'
                                     #                                      ,'qdap_value_negator'
                                     ,'qdap_value_ampl'
                                     ,'qdap_nword'  
                                     #                                      ,'sent_nneg','sent_npos','sent_negv'
                                     #                                      ,'sent_posv','sent_nwords','sent_val'
                                     #                                      ,'posextind','negextind','valuext'
                )
                
                for (i in 1:Narticle_issue){# i=1
                        text<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        #                         text=read.csv(paste(sFolderTexte,svFile[i],sep='')
                        #                                       ,stringsAsFactors=F
                        #                                       ,header=F
                        #                                       )
                        if(length(text)>1){text=text[2]}
                        text=gsub('\\t{1,}','',text)
                        #                         if (nrow(text)>1){
                        #                                 text=text[2,1]
                        #                         }
                        #                         chrtest=sapply(text,function(x) is.character(x))
                        text=paste(text,collapse=' ',sep='')
                        title_index=regexec(paste('keywords content=([^<]*)\\>?',sep=''),text)
                        #                                                 Ergebnis[i,'keywords']=regmatches(text,title_index)[[1]][2]
                        #                                                 \t\\t
                        df=data.frame(person='dirk',text=text)
                        tt=sentSplit(df,'text')[,3]                        
                        #                         tp1=polarity(tt,polarity.frame=pf,negators=negating)
                        #                         sent1=sum(tp1$all[c('polarity')]*tp1$all[c('wc')]^.5)
                        tp2=polarity(tt,polarity.frame=pf,negators=negating,amplifiers=amplifiers,deamplifiers=deamplifiers)
                        sent2=sum(tp2$all[c('polarity')]*tp2$all[c('wc')]^.5)
                        
                        nwords<-sum(tp2$all[c('wc')])
                        # sentiment ---------------------------------------------------------------
                        #                         sent3=sentiment(text,valueword)
                        #                         sentres=extracts(sent3)
                        Ergebnis[i,1:3]=c(id=gsub('article-|\\.txt','',svFile[i])
                                          #                                           ,sent1
                                          ,sent2
                                          ,nword=nwords 
                                          
                        )
                        
                        #                         Ergebnis[i,5:13]=unlist(sentres)
                        
                        #                                                 Ergebnis[i,'id']=gsub('article-|\\.txt','',svFile[i])
                        
                }
                write.csv(Ergebnis,paste(DirCode,'/data/zeit indikatoren/Ergebnis_qdap_ampl',liste_jahr[k],'.csv',sep=''),row.names=F)
        }
        
        rm(i) 
        
}#Jahresschleife
