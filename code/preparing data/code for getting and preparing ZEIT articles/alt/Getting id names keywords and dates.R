
# Beschreibung ------------------------------------------------------------

# Setting directories for storing files -------------------------------------------------------
# DirRawTexts="H:/Zeit" # text files are stored here
DirRawTexts="E:/Zeit" # text files are stored here
# DirRawTexts="C:/Users/Dirk/Documents/Zeit-Texte"

# DirCode='H:/git/zeit-2' # main directory
DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"
# setwd(DirCode)

# Load register created by 'Getting_register.R' ---------------------------
# load(paste(DirCode,"/register.RData",sep=''))



# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)

#  SentiWS: ------------------------------------------------------------------------
#       getting the list of positive and negative words and their values -----------------------------------------------------------------
#       valueword (lowercase) VALUEWORD (uppercase), created with valueword.R


# Texte eines Jahres laden -----------------------------------------------
for (jj in 1990:2015){#jj=1990 jj=2014
        # list of subdirectories each year
        liste_jahr=listsubdirs[grep(as.character(jj),listsubdirs)]
        for (k in 1:length(liste_jahr)){#k=1
                sFolderTexte=paste(DirRawTexts,'/',liste_jahr[k],'/',sep='')
                print(sFolderTexte)
                # getting list and number of articles
                svFile=list.files(sFolderTexte)
                svFile=svFile[-grep('article|plaintxt|Ergebnis',svFile)]
                
                mp=(1:length(svFile))%in%grep('-',svFile)
                sp=!mp
                svFile_sp=svFile[sp]
                
                art_num=gsub('.txt','',svFile_sp)
                Narticle_issue=length(svFile_sp)
                # initialize Results data.frame
                
                Ergebnis=data.frame(matrix(NA,length(art_num),4))
                colnames(Ergebnis)=c('title','number','date','keywords','link')
                Ergebnis[,'number']=art_num
                for (i in 1:Narticle_issue){# i=1
                        #                         text<-readLines(paste(sFolderTexte,svFile[i],sep=''), ok=F,encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        text=read.csv(paste(sFolderTexte,svFile_sp[i],sep=''),stringsAsFactors=F,header=F)
                        #                         if (nrow(text)>1){
                        #                                 text=text[2,1]
                        #                         }
                        text=paste(text,collapse='',sep='')
                        text=gsub('\\"||\\\\','',text)
                        
                        if (jj<=1995){
                                title_index=regexec(paste('<title>','(.*)',' DIE ZEIT Archiv',sep=''),text)
                                Ergebnis[i,'title']=regmatches(text,title_index)[[1]][2]
                        }
                        if (jj>1995){
                                title_index=regexec('headtitle-->, tt<title>(.*) \\| ZEIT ONLINE</title>, <!--end',text)
                                Ergebnis[i,'title']=regmatches(text,title_index)[[1]][2] 
                                
                        }
                        
                        
                        
                        
                        title_index=regexec(paste('keywords content=([^<]*)\\>?',sep=''),text)
                        Ergebnis[i,'keywords']=regmatches(text,title_index)[[1]][2]
                        #                         text[grep('keywords',text)]
                        # writeLines(text,'text.txt')
                        date_index=regexec(paste('date content=','([0-9]{4}-[0-9]{2}-[0-9]{2})',sep=''),text)
                        Ergebnis[i,'date']=regmatches(text,date_index)[[1]][2]
                        
                        
                        keywords_index=regexec(paste('content=(http://www.zeit.de/[0-9]{4}/[0-9]{2}/[^<]*)\\>?'),text)
                        Ergebnis[i,'link']=regmatches(text,keywords_index)[[1]][2]
                        
                }
                write.csv(Ergebnis,paste(DirCode,'/data/zeit indikatoren/meta_',jj,'_',k,'.csv',sep=''),row.names=F)
        }
        
        rm(i) 
        
}#Jahresschleife
#"supertitle"">Bank und Börse<span