# fuegt ratings und artikelinformationen zusammen

DirCode='H:/git/zeit-2' # main directory
# DirCode="C:/Users/Dirk/Documents/GitHub/zeit-2"
Dirlist=dir(paste(DirCode,'/data/zeit indikatoren',sep=''))
Dirlist=Dirlist[grep('Ergebnis_InflationWord',Dirlist)]
for (i in 1:length(Dirlist)){#i=1
        if(i==1){
                Ergebnis=read.csv(paste(paste(DirCode,'/data/zeit indikatoren/',Dirlist[i],sep='')))
                is=gsub('Ergebnis_InflationWord|\\.csv','',Dirlist[i])
                is=gsub('_','\\.',is,fixed=F)
                
                year=strsplit(is,'\\.')[[1]][1]
                issue=strsplit(is,'\\.')[[1]][2]
                Ergebnis[,'issue']=issue
                Ergebnis[,'year']=year
        }
        if(i>1){
                Erg=read.csv(paste(paste(DirCode,'/data/zeit indikatoren/',Dirlist[i],sep='')))
                is=gsub('Ergebnis_only_negation|\\.csv','',Dirlist[i])
                is=gsub('_','\\.',is,fixed=F)
                year=strsplit(is,'\\.')[[1]][1]
                issue=strsplit(is,'\\.')[[1]][2]
                Erg[,'issue']=issue
                Erg[,'year']=year               
                Ergebnis=rbind(Ergebnis,Erg)
                
                
        }
}
write.csv(Ergebnis,paste(DirCode,'/data/zeit indikatoren/InflationWord.csv',sep=''))
