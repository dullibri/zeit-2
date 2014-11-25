
# Beschreibung ------------------------------------------------------------

# dieser code liest zeit-artikel ein und bewertet sie anhand von SentiWS.
# 1. die anzahl der positiven und negativen Worten wird mit den Bewertungszahlen gewichtet und
#    aufsummiert
# 2. die anzahl der positiven und negativen worte wird berechnet
# 3. die bewertung auf positiv und negativ aufgesplittet wird angegeben.


# Letzte Modifikation -------------------------------------------------------------------

# 2014-06-20, 22:22

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep=''))

# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"

# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)

# SentiWS -----------------------------------------------------------------
library(tm)
# Positive und negative Wordlisten aufbereiten ----------------------------
sOutFolder='I:/Personal_Folders/employees/KX/A_MTI/Daten/Zeit/SentiWS_Bewertung/'
setwd('I:/Personal_Folders/employees/KX/A_MTI/Daten/SentiWS/')
sInFolder='I:/Personal_Folders/employees/KX/A_MTI/'
pos.words <- read.delim("SentiWS_v1.8c_Positive_ohne_NN.txt"
                        , encoding="UTF-8"
                        , header=F)

neg.words <- read.delim("SentiWS_v1.8c_Negative_ohne_NN.txt"
                        , encoding="UTF-8"
                        , header=F)

werte_pos=pos.words[,3,drop=F]
werte_neg=neg.words[,3,drop=F]


# Aufbereiten der Lexica --------------------------------------------------

# Wortgruppen zusammenführen (Abbau[,1], abbauen, Abbaus, ...[,4] ---------
NEG=matrix(NA,nrow=nrow(neg.words),ncol=1)
for (i in 1:nrow(neg.words)){NEG[i,]=paste(neg.words[i,1],neg.words[i,4],sep=',')}
NEG=tolower(NEG)
NEG <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", NEG)
NEG=strsplit(NEG,',')

POS=matrix(NA,nrow=nrow(pos.words),ncol=1)
for (i in 1:nrow(pos.words)){POS[i,]=paste(pos.words[i,1],pos.words[i,4],sep=',')}
POS=tolower(POS)
POS <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", POS)
POS=strsplit(POS,',')

# NICHT EINDEUTIGE WORTE RAUS ---------------------------------------------

doppel_pos=match(POS,NEG)
doppel_neg=match(NEG,POS)

which(is.na(doppel_pos)==F)
POS[which(is.na(doppel_pos)==F)]
POS=POS[-which(is.na(doppel_pos)==F)]

which(is.na(doppel_neg)==F)
NEG[which(is.na(doppel_neg)==F)]
NEG=NEG[-which(is.na(doppel_neg)==F)]

# Eine Gesamtliste positiver und negativer Worte und bewertungen -----------------
POSNEG=c(POS,NEG)
posneg.words=rbind(pos.words,neg.words)


# zeit nehmen -------------------------------------------------------------
begin_test=proc.time()

# Texte eines Jahres laden -----------------------------------------------

for (jj in 2013:2013){#jj=2001
  
sFolderTexte=paste(sInFolder,'Daten/Zeit/',jj,'/',sep='')
# liste_jahr=list.files(sFolderTexte)
liste_jahr=listsubdirs[grep(as.character(1990),listsubdirs)]
for (k in 1:length(liste_jahr)){#k=1
  sFolderTexte=paste(DirRawTexts,'/',liste_jahr[k],'/',sep='')
  svFile=list.files(sFolderTexte)
  svFile=svFile[grep('article',svFile)]
  Narticle_issue=length(svFile)
  rohtext=list()
  for (i in 1:Narticle_issue){#i=1
          aux<-read.csv(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8", header=T,stringsAsFactors =F)
                                         
                                                                         
          if (length(aux)==1){
                  rohtext[svFile[i]]=names(aux)
          }else{rohtext[svFile[i]]=aux[2]}
          
  }
  rm(i) 

# DIE SCHLEIFE GEHT BIS ZUM ENDE DURCH



# Corpus anlegen ----------------------------------------------------------
docs <- Corpus(VectorSource(rohtext), readerControl = list(language = "de"))

# docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation,preserve_intra_word_dashes = T)
docs <- tm_map(docs, removeWords, stopwords("german"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, c("</p>","/","@","\\|"))

# Notwendiges entfernen von /, @, \\, damit diese nicht zwei Worte --------
# for (j in seq(docs))
# {
# docs[[j]] <- gsub("</p>", "", docs[[j]])
# docs[[j]] <- gsub("/", "", docs[[j]])
# docs[[j]] <- gsub("@", "", docs[[j]])
# docs[[j]] <- gsub("\\|", "", docs[[j]])
# docs[[j]] <- gsub("pstrong", "", docs[[j]])
# docs[[j]] <- gsub("strong", "", docs[[j]])
# }



# Document Term Matrix erstellen ------------------------------------------
dtm <- DocumentTermMatrix(docs)
Mdtm=as.matrix(dtm)

# Welche reihennummer haben in POSNEG haben die relevanten spalten in der dtm ---------------------------------
t9=match(colnames(dtm),POSNEG)
#posneg.words[2332,]

# dtm auf relevante verkleinern -------------------------------------------
Mdtm_rel=Mdtm[,is.na(t9)==F]
rel_words=as.matrix(colnames(Mdtm_rel))

# spalten-worte den POSNEG zuordnen ---------------------------------------
test=match(POSNEG,rel_words) # die nicht negativen werte sind die zeilennummern von rel_words
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
# write.csv(Ergebnis,paste(sOutFolder,liste_jahr[k],' Ergebnis.csv',sep=''))

}#Ausgabenschleife
}#Jahresschleife
# zeitnehmen abschließen --------------------------------------------------
end_test=proc.time()
d=end_test-begin_test
