#  ------------------------------------------------------------------------
# This file converts plainhtml to plain text and stores the files ---------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep=''))

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

listsubdirs=list.files(DirRawTexts)

for (subd in listsubdirs[1:1281]){
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
        for (file in listfiles){
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
DirCode='H:/git/zeit-2'
# load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"


# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)



for (subd in listsubdirs[1:1281]){
        
        # List of documents in plaintext ------------------------------------------
        listfiles=dir(paste(DirRawTexts,'/',subd,sep=''))
        listplaintexts=listfiles[grep('plaintxt',listfiles)]
        ids=gsub('plaintxt','',listplaintexts)
        ids=gsub('.txt','',ids)
        ids=gsub('^-','',ids)
        
        
        
        # Texts with multiple pages -----------------------------------------------
        mtext=ids[grep('-',ids)]
        
        mtext=strsplit(mtext,'-')
        mtext=t(sapply(mtext,function(x){as.numeric(x[1:2])}))
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
                          paste(DirRawTexts,'/',subd,'/','article-',stext[i],'.txt',sep=''))
        }
        rm(i)
        
        
        # m-pager: aggregate and save them as articles -------------------------------------------------
        if (!length(mtext)==0){
                for (i in 1:length(idsm)){
                        nidsm=nrow(mtext[mtext[,1]==idsm[i],])
                        article=character(nidsm)
                        for (page in 1:nidsm){
                                
                                article[page]=readLines(paste(DirRawTexts,'/',subd,'/','plaintxt-',idsm[i],'-',page,'.txt',sep=''),encoding='UTF-8')
                        }
                        article=paste(article,sep="",collapse="")
                        if (nchar(as.character(register$title[idsm[i]]))<120){
                                article=gsub(register$title[idsm[i]],'',article)
                        }
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

# 2014-11-25, 17:14
# 2014-06-20, 22:22

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
setwd(DirCode)
# load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"

# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)

# SentiWS -----------------------------------------------------------------
library(tm)

# Positive und negative Wordlisten aufbereiten ----------------------------

sDirS='/SentiWS_v1.8c/'

pos.words <- read.delim(paste(DirCode,sDirS,"SentiWS_v1.8c_Positive_without_pipes.txt",sep='')# pipes have been eliminated with excel.
                        , encoding="UTF-8"
                        , header=F)

neg.words <- read.delim(paste(DirCode,sDirS,"SentiWS_v1.8c_Negative_without_pipes.txt",sep='')
                        , encoding="UTF-8"
                        , header=F)

# Aufbereiten der Lexica --------------------------------------------------

# Wortgruppen zusammenführen (Abbau[,1], abbauen, Abbaus, ...[,4] ---------
NEG=matrix(NA,nrow=nrow(neg.words),ncol=1)
for (i in 1:nrow(neg.words)){NEG[i,]=paste(neg.words[i,1],neg.words[i,4],sep=',')}
nn=sapply(NEG,paste,sep='',collapse=',')
nnn=paste(nn,sep='',collapse=',')
nnnn=strsplit(nnn,',')
nnnn=sapply(nnnn,function(x) x)

NEG <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", NEG)
NEG=strsplit(NEG,',')

POS=matrix(NA,nrow=nrow(pos.words),ncol=1)
for (i in 1:nrow(pos.words)){POS[i,]=paste(pos.words[i,1],pos.words[i,4],sep=',')}
pp=sapply(POS,paste,sep='',collapse=',')
ppp=paste(pp,sep='',collapse=',')

pppp=strsplit(ppp,',')
pppp=sapply(pppp,function(x) x)
# ind=grep('',pppp[,1])
pppp=pppp[pppp[,1]!='',drop=F]
# tt=paste(ppp,',')

uneindeutig=intersect(nnnn,pppp)

POS <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", POS)
POS=unlist(strsplit(POS,','))

# NICHT EINDEUTIGE WORTE RAUS ---------------------------------------------
POS=tolower(POS)
NEG=tolower(NEG)
doppel_pos=match(POS,NEG)
doppel_pos=doppel_pos[is.na(doppel_pos)==F]
doppel_neg=match(NEG,POS)
doppel_neg=doppel_neg[is.na(doppel_neg)==F]

POS=POS[-doppel_pos]
pos.words=pos.words[-doppel_pos,]

NEG=NEG[-doppel_neg]
neg.words=neg.words[-doppel_neg,]

# Eine Gesamtliste positiver und negativer Worte und bewertungen -----------------
POSNEG=c(POS,NEG)

posneg.words=rbind(pos.words,neg.words)
posneg.words[1,1]<-gsub('<U+FEFF>','',posneg.words[1,1])
posneg.words[,1]=tolower(posneg.words[,1])
posneg.words[,4]=tolower(posneg.words[,4])
# posneg=Corpus(VectorSource(as.character(posneg.words[,1])),readerControl = list(language = "de"))
# posneg=tm_map(posneg,stemDocument)
# 
# pn <- DocumentTermMatrix(posneg)
# mpn=as.matrix(pn)
# 
# ipn=colnames(mpn)
# cols=colSums(mpn)
# doppelte=ipn[which(cols>1)]
# zeit nehmen -------------------------------------------------------------
begin_test=proc.time()

# Texte eines Jahres laden -----------------------------------------------

for (jj in 1990:2014){#jj=1990
        
        liste_jahr=listsubdirs[grep(as.character(1990),listsubdirs)]
        for (k in 1:length(liste_jahr)){#k=1
                sFolderTexte=paste(DirRawTexts,'/',liste_jahr[k],'/',sep='')
                svFile=list.files(sFolderTexte)
                svFile=svFile[grep('article',svFile)]
                Narticle_issue=length(svFile)
                rohtext=character(Narticle_issue)
                for (i in 1:Narticle_issue){#)
                        aux<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        if (length(aux)==1){rohtext[i]=aux}else{rohtext[i]=aux[2]}                        
                }
                rm(i) 
                rohtext=gsub('""',' ',rohtext) # This is to avoid unvoluntarily adding two words when replacing punctuation.
                
                
                
                
                # Corpus anlegen ----------------------------------------------------------
                docs <- Corpus(VectorSource(rohtext), readerControl = list(language = "De"))
                
                # docs <- tm_map(docs, tolower) # does not matter, as document term matrix will eliminate capital letters
                docs <- tm_map(docs, removeNumbers)
                docs <- tm_map(docs, removePunctuation,preserve_intra_word_dashes = T)
                docs <- tm_map(docs, removeWords, stopwords("german"))
                docs <- tm_map(docs, stripWhitespace)
                docs <- tm_map(docs, removeWords, c("</p>","/","@","\\|"))
                # docs <- tm_map(docs,stemDocument) # there are most permutations listed, which makes discerning between different
                # parts of speech easier.
                
                
                # Which influence has not taking account of capital letters ---------------
                #     VORHER MUSS tolower(POSNEG,posneg.words) abgestellt werden
                # test <-tolower(POSNEG)
                #                 test <- Corpus(VectorSource(test), readerControl = list(language = "De"))
                #                 testdtm<- DocumentTermMatrix(test)
                #                 testMdtm<-as.matrix(testdtm)
                #                 ungenau=which(testMdtm>1)
                #                 ungenau_words=colnames(testMdtm)[ungenau]
                #                 ungenau_words=gsub(',','',ungenau_words)
                #                 write.csv(ungenau_words,'test.txt')# comparison reveals, that this is no problem
                
                # Are there any words or permuations with 2 or more values attributed -------
                # (due to different parts of speech, e.g.)
                #                 errorq=function(x){position=grep(x,POSNEG)
                #                                    return(posneg.words[position[1],3]-posneg.words[position[2],3])}
                #                 tt=sapply(ungenau_words,errorq)
                # grep('schwächst',neg.words[,4])
                # neg.words[1371,]
                # neg.words[1364,]
                # write.csv(ungenau_words,'test.txt')
                # some, in total 128 double entries. however, only some permutions.
                
                # Document Term Matrix erstellen ------------------------------------------
                dtm <- DocumentTermMatrix(docs)
                Mdtm=as.matrix(dtm)
                
                
                # Welche reihennummer haben in POSNEG haben die relevanten spalten in der dtm ---------------------------------
                t9=match(colnames(dtm),POSNEG)
                
                # dtm auf relevante verkleinern -------------------------------------------
                Mdtm_rel=Mdtm[,is.na(t9)==F]
                rel_words=as.matrix(colnames(Mdtm_rel))
                
                # spalten-worte den POSNEG zuordnen ---------------------------------------
                test=match(POSNEG,rel_words) # die existierenden werte sind die zeilennummern von rel_words
                tt=test[is.na(test)==F]
                t1=c(1:length(POSNEG))[is.na(test)==F] #  das sind die dazugehörigen spaltennumern von POSNEG
                t2=data.frame(zeile_von_Mdtm_rel_in_POSNEG=test[t1],zeile_posneg=t1) # die beiden zu einem dataframe
                t2=t2[with(t2,order(zeile_von_Mdtm_rel_in_POSNEG)),] # nach spaltennummern er Mdtm_rel, d.h. rel_words zeilennummern sortieren
                
                # anzahl mal bewertung -----------------------------------------------------
                t3=apply(Mdtm_rel,1,function(x,y){x*y},y=posneg.words[t2[,2],3]) # anzahl rel_words X anzahl artikel
                t4=as.matrix(colSums(t3))
                Ergebnis=data.frame(Bewertung=t4)
                
                # wieviele positive, wieviele negative ------------------------------------
                t2$pos=t2$zeile_posneg<(nrow(pos.words)+1)
                Ergebnis$Anzahl_pos_worte=as.matrix(rowSums(Mdtm_rel[,t2$pos]))
                Ergebnis$Anzahl_neg_worte=as.matrix(rowSums(Mdtm_rel[,t2$pos==F]))
                
                # Nur positive, nur negative bewertung ------------------------------------
                
                # positive bewertung: anzahl mal bewertung -----------------------------------------------------
                t3=apply(Mdtm_rel[,t2$pos],1,function(x,y){x*y},y=posneg.words[t2[t2$pos,2],3]) # anzahl rel_words X anzahl artikel
                t4=as.matrix(colSums(t3))
                Ergebnis$Nur_positive_Bewertung=t4
                
                # negative bewertung: anzahl mal bewertung -----------------------------------------------------
                t3=apply(Mdtm_rel[,t2$pos==F],1,function(x,y){x*y},y=posneg.words[t2[t2$pos==F,2],3]) # anzahl rel_words X anzahl artikel
                t4=as.matrix(colSums(t3))
                Ergebnis$Nur_negative_Bewertung=t4
                
                
                # Anzahl der Worte --------------------------------------------------------
                
                Ergebnis$Anzahl_der_Worte=rowSums(Mdtm)
                
                Ergebnis$pos_und_neg_durch_anzahl=(Ergebnis$Anzahl_pos_worte+Ergebnis$Anzahl_neg_worte)/Ergebnis$Anzahl_der_Worte*100
                article_num=gsub('article-','',svFile)
                article_num=gsub('.txt','',article_num)
                Ergebnis$article_id=article_num
                write.csv(Ergebnis,paste(sFolderTexte,' Ergebnis.csv',sep=''))
                
        }#Ausgabenschleife
}#Jahresschleife
# zeitnehmen abschließen --------------------------------------------------
end_test=proc.time()
d=end_test-begin_test



