
# Beschreibung ------------------------------------------------------------

# dieser code liest zeit-artikel ein und bewertet sie anhand von SentiWS.
# 1. die anzahl der positiven und negativen Worten wird mit den Bewertungszahlen gewichtet und
#    aufsummiert
# 2. die anzahl der positiven und negativen worte wird berechnet
# 3. die bewertung auf positiv und negativ aufgesplittet wird angegeben.


# Letzte Modifikation -------------------------------------------------------------------
# 2014-12-01, 17:08
# 2014-11-25, 17:14
# 2014-06-20, 22:22

# Load the necessary libraries and defining functions
library(tm)
gross=function(x){x=grep('([A-ZÖÄÜ][a-zäüöß]+)',x)} # returns a vector of the uppercase words in a char vector



# Setting directories for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit" # text files are stored here
DirCode='H:/git/zeit-2' # main directory
setwd(DirCode)

# Load register created by 'Getting_register.R' ---------------------------
# load(paste(DirCode,"/register.RData",sep=''))

# Getting subdirectories --------------------------------------------------
# listsubdirs=list.files(DirRawTexts)

#  SentiWS: ------------------------------------------------------------------------
#       getting the list of positive and negative words and their values -----------------------------------------------------------------
#       posneg (lowercase) POSNEG (uppercase)
# source('Preparing SentiWs.R')
load(paste(DirCode,"/SentiWS.RData",sep=''))
# source(paste(DirCode,"/Preparing SentiWs.R",sep='')) # to make SentiWS.RData
source(paste(DirCode,"/valueword_capital_letters.R",sep=''))
source(paste(DirCode,"/valueword.R",sep=''))

# taking time -------------------------------------------------------------
begin_test=proc.time()

vposneg=paste(posneg[,2],collapse='',sep='')
vposneg=sapply(strsplit(vposneg,','),function(x) x)

vPOSNEG=paste(POSNEG[,2],collapse='',sep='')
vPOSNEG=tolower(vPOSNEG)
vPOSNEG=sapply(strsplit(vPOSNEG,','),function(x) x)
vPOSNEG[1]='abmachung'

# Texte eines Jahres laden -----------------------------------------------
for (jj in 2003){#jj=1990
        
        liste_jahr=listsubdirs[grep(as.character(jj),listsubdirs)]
        for (k in 1:length(liste_jahr)){#k=1
                sFolderTexte=paste(DirRawTexts,'/',liste_jahr[k],'/',sep='')
                print(sFolderTexte)
                svFile=list.files(sFolderTexte)
                svFile=svFile[grep('article',svFile)]
                Narticle_issue=length(svFile)
                if (Narticle_issue==1){
                        print(paste('ACHTUNG: ',sFolderTexte,' hat nur einen Text'))        
                        next
                }
                rohtext=character(Narticle_issue) # upper case words of the respective texts
                Rohtext=character(Narticle_issue) # lower case words 
                article_ids=character(Narticle_issue)
                for (i in 1:Narticle_issue){#)
                        aux<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        article_id=gsub('article-|.txt','',svFile[i])
                        article_ids[i]=article_id
                        if (length(aux)>1&nchar(aux[1])<15){aux=aux[2]}
                        # This splits rohtext[1] into lower- (roh) and uppercase (Roh) words
                        aux_split=strsplit(aux,' ')
                        #                         aux_split=sapply(aux_split,function(x) x)
                        Roh_ind=sapply(aux_split,gross)
                        roh_ind=c(1:length(Roh_ind))[-Roh_ind]
                        rohtext[i]=paste(sapply(roh_ind,function(x) aux_split[[1]][x] )
                                         ,collapse=' '
                                         ,sep=' ')
                        Rohtext[i]=paste(sapply(Roh_ind,function(x) aux_split[[1]][x])   
                                         ,collapse=' '
                                         ,sep=' ')
                }
                rm(i) 
                
                
                
                
                # Corpus anlegen ----------------------------------------------------------
                docs <- Corpus(VectorSource(rohtext), readerControl = list(language = "De"))
                
                docs <- tm_map(docs, tolower) 
                docs <- tm_map(docs, removeNumbers)
                docs <- tm_map(docs, removePunctuation,preserve_intra_word_dashes = T)
                docs <- tm_map(docs, removeWords, stopwords("german"))
                docs <- tm_map(docs, stripWhitespace)
                docs <- tm_map(docs, removeWords, c("</p>","/","@","\\|"))
                # docs <- tm_map(docs,stemDocument) # there are most permutations listed, which makes discerning between different
                # parts of speech easier.
                
                DOCS <- Corpus(VectorSource(Rohtext), readerControl = list(language = "De"))
                
                DOCS <- tm_map(DOCS, tolower) 
                DOCS <- tm_map(DOCS, removeNumbers)
                DOCS <- tm_map(DOCS, removePunctuation,preserve_intra_word_dashes = T)
                DOCS <- tm_map(DOCS, removeWords, stopwords("german"))
                DOCS <- tm_map(DOCS, stripWhitespace)
                DOCS <- tm_map(DOCS, removeWords, c("</p>","/","@","\\|"))
                
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
                
                
                # lower case --------------------------------------------------------------
                #  ------------------------------------------------------------------------
                #  ------------------------------------------------------------------------               
                
                # Document Term Matrix erstellen ------------------------------------------
                
                dtm <- DocumentTermMatrix(docs)
                Mdtm=as.matrix(dtm)
                
                
                # Welche reihennummer haben in vposneg haben die relevanten spalten in der dtm ---------------------------------
                t9=match(colnames(dtm),vposneg)
                
                
                
                # dtm auf relevante verkleinern -------------------------------------------
                Mdtm_rel=Mdtm[,is.na(t9)==F]
                rel_words=as.matrix(colnames(Mdtm_rel))    
                
                # spalten-worte den POSNEG zuordnen ---------------------------------------
                #                 test=match(valueword[,2],rel_words) # die existierenden werte sind die zeilennummern von rel_words
                #                 test=test[is.na(test)!=T]
                ind_valueword=lapply(rel_words,function(x) which(x==valueword[,1]))
                ind_valueword=sapply(ind_valueword,function(x) x[[1]])
                value=valueword[ind_valueword,2]
                ind_pos=value>0
                ind_neg=value<0
                
                Ergebnis=data.frame(
                        npword=rowSums(Mdtm_rel[,ind_pos])
                        ,nnword=rowSums(Mdtm_rel[,ind_neg])
                        ,nword=rowSums(Mdtm)
                )      
                
                
                #                 value_article=Mdtm_rel%*%value
                pvalue=value
                pvalue[ind_neg]=0
                nvalue=value
                nvalue[ind_pos]=0
                Ergebnis$pvalue=Mdtm_rel%*%pvalue
                Ergebnis$nvalue=Mdtm_rel%*%nvalue
                
                
                # upper case --------------------------------------------------------------
                #  ------------------------------------------------------------------------
                #  ------------------------------------------------------------------------
                
                
                
                # Document Term Matrix erstellen ------------------------------------------
                
                
                
                
                dtm <- DocumentTermMatrix(DOCS)
                Mdtm=as.matrix(dtm)
                
                
                
                # Welche reihennummer haben in vposneg haben die relevanten spalten in der dtm ---------------------------------
                t9=match(colnames(dtm),vPOSNEG)
                
                
                Mdtm_rel=Mdtm[,is.na(t9)==F]
                rel_words=as.matrix(colnames(Mdtm_rel))    
                
                
                
                # spalten-worte den POSNEG zuordnen ---------------------------------------
                #                 test=match(valueword[,2],rel_words) # die existierenden werte sind die zeilennummern von rel_words
                #                 test=test[is.na(test)!=T]
                ind_valueword=lapply(rel_words,function(x) which(x==VALUEWORD[,1]))
                ind_valueword=sapply(ind_valueword,function(x) x[[1]])
                value=VALUEWORD[ind_valueword,2]
                ind_pos=value>0
                ind_neg=value<0
                
                ERGEBNIS=data.frame(
                        npword=rowSums(Mdtm_rel[,ind_pos])
                        ,nnword=rowSums(Mdtm_rel[,ind_neg])
                        ,nword=rowSums(Mdtm)
                )
                #                 value_article=Mdtm_rel%*%value
                pvalue=value
                pvalue[ind_neg]=0
                nvalue=value
                nvalue[ind_pos]=0
                ERGEBNIS$pvalue=Mdtm_rel%*%pvalue
                ERGEBNIS$nvalue=Mdtm_rel%*%nvalue
                Ergebnis=Ergebnis+ERGEBNIS
                
                Ergebnis=cbind(id=article_ids,Ergebnis)
                
                write.csv(Ergebnis,paste(sFolderTexte,'Ergebnis.csv',sep=''))
                write.csv(ERGEBNIS,paste(sFolderTexte,'Ergebnis_capital.csv',sep=''))
        }#Ausgabenschleife
}#Jahresschleife
# zeitnehmen abschließen --------------------------------------------------
end_test=proc.time()
d=end_test-begin_test



