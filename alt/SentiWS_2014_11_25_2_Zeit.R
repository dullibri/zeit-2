
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
load(paste(DirCode,"/register.RData",sep=''))

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

pos.words[,1]=tolower(pos.words[,1])
pos.words[,4]=tolower(pos.words[,4])


neg.words[,1]=tolower(neg.words[,1])
neg.words[,4]=tolower(neg.words[,4])
posneg.words=rbind(pos.words,neg.words)
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

POS <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", POS)
POS=unlist(strsplit(POS,','))


# NICHT EINDEUTIGE WORTE RAUS ---------------------------------------------



ndoppelte=nnnn[duplicated(nnnn)]

tfind=sapply(ndoppelte,function(x)if(length(x)>2){return(1)}else{return(0)})
ndoppelte=ndoppelte[tfind==1]

# grep('klagen',NEG) um das zu testen.
for (i in ndoppelte){
        NEG=gsub(i,'',NEG)
        posneg.words[,1]=gsub(i,'',posneg.words[,1])
        posneg.words[,4]=gsub(i,'',posneg.words[,4])
}


# positive ----------------------------------------------------------------

pdoppelte=pppp[duplicated(pppp)]

tfind=sapply(pdoppelte,function(x)if(length(x)>2){return(1)}else{return(0)})
pdoppelte=pdoppelte[tfind==1]

# grep('klagen',NEG) um das zu testen.
for (i in pdoppelte){
        POS=gsub(i,'',POS)
        posneg.words[,1]=gsub(i,'',posneg.words[,1])
        posneg.words[,4]=gsub(i,'',posneg.words[,4])
}



uneindeutig=intersect(nnnn,pppp)
for (i in uneindeutig){
        POS=gsub(i,'',POS)
        NEG=gsub(i,'',NEG)
        posneg.words[,1]=gsub(i,'',posneg.words[,1])
        posneg.words[,4]=gsub(i,'',posneg.words[,4])
}

POSNEG=matrix(NA,nrow=nrow(posneg.words),ncol=1)
for (i in 1:nrow(posneg.words)){POSNEG[i,]=paste(posneg.words[i,1],posneg.words[i,4],sep=',')}



POSNEG <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", POSNEG)
# POSNEG=unlist(strsplit(POSNEG,','))
POSNEG=strsplit(POSNEG,',')




# zeit nehmen -------------------------------------------------------------
begin_test=proc.time()

# Texte eines Jahres laden -----------------------------------------------

for (jj in 2007:2014){#jj=1990
        
        liste_jahr=listsubdirs[grep(as.character(jj),listsubdirs)]
        for (k in 1:length(liste_jahr)){#k=1
                sFolderTexte=paste(DirRawTexts,'/',liste_jahr[k],'/',sep='')
                svFile=list.files(sFolderTexte)
                svFile=svFile[grep('article',svFile)]
                Narticle_issue=length(svFile)
                rohtext=character(Narticle_issue)
                for (i in 1:Narticle_issue){#)
                        aux<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        if (length(aux)==1){rohtext[i]=aux}
                        else{
                                rohtext[i]=aux[2]
                        }                        
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
#                 test=paste(POSNEG,sep='',collapse='')
#                 t9=match(colnames(dtm),test)
                # dtm auf relevante verkleinern -------------------------------------------
                Mdtm_rel=Mdtm[,is.na(t9)==F,drop=F]
                rel_words=as.matrix(colnames(Mdtm_rel))
                
                # spalten-worte den POSNEG zuordnen ---------------------------------------
                test=match(POSNEG,rel_words) # die existierenden werte sind die zeilennummern von rel_words
#                 tt=test[is.na(test)==F]
                t1=c(1:length(POSNEG))[is.na(test)==F] #  das sind die dazugehörigen spaltennumern von POSNEG
                t2=data.frame(zeile_von_Mdtm_rel_in_POSNEG=test[t1],zeile_posneg=t1) # die beiden zu einem dataframe
                t2=t2[with(t2,order(zeile_von_Mdtm_rel_in_POSNEG)),] # nach spaltennummern er Mdtm_rel, d.h. rel_words zeilennummern sortieren
                
                # anzahl mal bewertung -----------------------------------------------------
                t3=apply(Mdtm_rel,1,function(x,y){x*y},y=posneg.words[t2[,2],3]) # anzahl rel_words X anzahl artikel
                t4=as.matrix(colSums(t3))
                Ergebnis=data.frame(Bewertung=t4)
                
                # wieviele positive, wieviele negative ------------------------------------
                t2$pos=t2$zeile_posneg<(nrow(pos.words)+1)
                Ergebnis$Anzahl_pos_worte=as.matrix(rowSums(Mdtm_rel[,t2$pos,drop=F]))


                Ergebnis$Anzahl_neg_worte=as.matrix(rowSums(Mdtm_rel[,t2$pos==F,drop=F]))
                
                # Nur positive, nur negative bewertung ------------------------------------
                
                # positive bewertung: anzahl mal bewertung -----------------------------------------------------
                t3=apply(Mdtm_rel[,t2$pos,drop=F],1,function(x,y){x*y},y=posneg.words[t2[t2$pos,2],3]) # anzahl rel_words X anzahl artikel
                t4=as.matrix(colSums(t3))
                Ergebnis$Nur_positive_Bewertung=t4
                
                # negative bewertung: anzahl mal bewertung -----------------------------------------------------
                t3=apply(Mdtm_rel[,t2$pos==F,drop=F],1,function(x,y){x*y},y=posneg.words[t2[t2$pos==F,2],3]) # anzahl rel_words X anzahl artikel
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
