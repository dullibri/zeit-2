
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




DirCode='H:/git/zeit-2' # main directory
# DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"
setwd(DirCode)

# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)

# Texte eines Jahres laden -----------------------------------------------
for (jj in 2015:2015){#jj={#:2015 jj=1990 jj=2014
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
                
                Ergebnis=data.frame(matrix(NA,1,3))
                colnames(Ergebnis)=c('id','Inflation','InflationMutationen')
                
                for (i in 1:Narticle_issue){# i=1
                        
                        text<-readLines(paste(sFolderTexte,svFile[i],sep=''), encoding="UTF-8")#, header=T,stringsAsFactors =F)
                        #                         text=read.csv(paste(sFolderTexte,svFile[i],sep='')
                        #                                       ,stringsAsFactors=F
                        #                                       ,header=F
                        #                                       )
                        if(length(text)>1){text=text[2]}
                        text=gsub('\\t{1,}|/|\\||www\\.[a-zA-Z0-9]{1,40}\\.de|[0-9]{1,}','',text)
                        text=gsub('\\s{2,}','',text)
                        text=gsub('\\s{1,1}\\.','\\.',text)
                        
                      
                        
                                                
                        
                        # sentiment ---------------------------------------------------------------
                        
                        Ergebnis[i,1:3]=c(id=gsub('article-|\\.txt','',svFile[i])
                                          ,if(length(grep('Inflation',text,fixed=F)>0)){Inflation=1}else{Inflation=0}
                                          ,if(length(grep('[Ii]nflation',text)>0)){InflationMutationen=1}else{InflationMutationen=0}
                    

                                          
                        )
                  
                        
                        #                                                 Ergebnis[i,'id']=gsub('article-|\\.txt','',svFile[i])
                        
                }
                write.csv(Ergebnis,paste(DirCode,'/data/zeit indikatoren/Ergebnis_InflationWord',liste_jahr[k],'.csv',sep=''),row.names=F)
        }
        
        rm(i) 
        
}#Jahresschleife


