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







