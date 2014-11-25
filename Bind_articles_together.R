#  ------------------------------------------------------------------------
# This file puts the different plaintexts of each article together --------
#  ------------------------------------------------------------------------

# Load register created by 'Getting_register.R' ---------------------------
DirCode='H:/git/zeit-2'
load(paste(DirCode,"/register.RData",sep=''))
     
# Setting directory for storing files -------------------------------------------------------
DirRawTexts="H:/Zeit"


# Getting subdirectories --------------------------------------------------
listsubdirs=list.files(DirRawTexts)
 


for (subd in listsubdirs){
        
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
        
        idsm=unique(mtext[,1])
        idsm=sort(idsm)
        
        # Files with just one page ------------------------------------------------
        stext=as.numeric(ids[-grep('-',ids)])
        stext=stext[(stext%in%idsm)==0]
        stext=sort(stext)
        # onepager: copy plaintexts into articles  --------------------------------
        for (i in 1:length(stext)){
                file.copy(paste(DirRawTexts,'/',subd,'/','plaintxt-',stext[i],'.txt',sep=''),
                          paste(DirRawTexts,'/',subd,'/','article-',stext[i],'.txt',sep=''))
        }
        rm(i)
        
        # m-pager: aggregate and save them as articles -------------------------------------------------
        
        for (i in 1:length(idsm)){
                nidsm=nrow(mtext[mtext[,1]==idsm[i],])
                article=character(nidsm)
                for (page in 1:nidsm){
                        
                        article[page]=readLines(paste(DirRawTexts,'/',subd,'/','plaintxt-',idsm[i],'-',page,'.txt',sep=''),encoding='UTF-8')
                }
                article=paste(article,sep="",collapse="")
                article=gsub(register$title[idsm[i]],'',article)
                write.csv(article,
                          paste(DirRawTexts,'/',subd,'/','article-',idsm[i],'.txt',sep='')
                )
        }
}
rm(i,j)
# sapply(mtext,strsplit,'-')







