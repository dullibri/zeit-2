# Verbesserung: exp, exp.pr, im und im.pr aus CA Datei. Empl direkt aus alt_ind - up-daten

# notwendig, um den pfad zu finden.
info<-list()
info$Sfile='I:/Personal_Folders/employees/KX/A_MTI/'
info$SfileCode='I:/Personal_Folders/employees/KX/A_MTI/Code/'
info$SfileData='I:/Personal_Folders/employees/KX/A_MTI/Daten/MF/'
DirCode='H:/git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'

# Alternative Indikatoren laden -------------------------------------------------------------

df <- read.csv(paste(DirCode,'/Data/alternative_indicators_long.csv',sep='')
                ,row.names=1 
                ,header=T)


df$spread=df$long.rate-df$short.rate


# renaming row.names ------------------------------------------------------
row.names(df)=gsub('15/','',row.names(df))
rn.split=strsplit(row.names(df),'/')
rn.new=sapply(rn.split,function(x) paste(x[2],x[1],sep='/'))
row.names(df)=rn.new
rm(rn.new,rn.split)
# Einzelne Variablen rausnehmen -------------------------------------------

df$m0=NULL
df$m1=NULL
df$m2=NULL
df$m3=NULL
df$gold=NULL
df$eer=NULL
# df$u=NULL
df$constr.order=NULL
df$prod=NULL
df$wholesale=NULL
df$retail.sales=NULL
df$bus.exp=NULL

# Hinzufuegen der Mediatenor daten -----------------------------------------

mt <- read.csv(paste(info$Sfile,'Daten/MediaTenor/MediaTenor_indicators_2001m01_2014m04.csv',sep=''))

# herausnehmen der indikatoren --------------------------------------------

vSel_c = which(colSums(is.na(mt))<2)
mt_small = mt[,vSel_c]
rm(vSel_c)

rn.split=strsplit(as.character(mt_small$Year_Month),'_')
rn.new=sapply(rn.split,function(x) paste(x[1],x[2],sep='/'))
row.names(mt_small)=rn.new
rm(rn.split,rn.new)
mt_small=mt_small[,4:ncol(mt_small)]

df[,colnames(mt_small)]=NA
df[row.names(mt_small),colnames(mt_small)]=mt_small
df$MTI_Past=NULL # NAs


# idiosyncratische komponente und 1. HK ------------------------------------

# MFread_data bis mt_small laufen alssen
# row.names(mt_small)=mt[,3] # Zeit als Zeilenbeschreibung
rm(mt)
mt_small=mt_small[1:(nrow(mt_small)-1),]# für April 2014 sind keine Daten vorhanden
# dif_mt_small=mt_small[2:nrow(mt_small),]-mt_small[1:(nrow(mt_small)-1),] # Erste Differenzen
PcMti=prcomp(mt_small
# PcMti=prcomp(dif_mt_small # falls in Differenzen
             ,center = TRUE # mean=0
             #               ,scaled=T # sd=1
)

mt_loadings=as.matrix(PcMti$rotation)
# test1=predict(PcMti)%*%matrix(mt_loadings[1,],ncol=1) #so bekommt man die orginalreihe aus den pc und den loadings
# test2=predict(PcMti)[,2:ncol(mt_loadings)]%*%matrix(mt_loadings[1,2:ncol(mt_loadings)],ncol=1) # originalreihe nur mit den letzten hk
mt_pc_level=as.matrix(mt_small)%*%mt_loadings # pcs der level daten mit den loadings der differenzen
mt_idio=matrix(NA,nrow(mt_small),ncol(mt_small))
for (i in 1:ncol(mt_small)){
  mt_idio[,i]=lm(mt_small[,i]~mt_pc_level[,1])$res
}
rm(i)
# mt_idio_alternativ=mt_pc_level[,2:ncol(mt_pc_level)]%*%t(mt_loadings[,2:ncol(mt_loadings)]) # ohne die erste hk wieder zusammensetzen

colnames(mt_idio)=paste('idio',colnames(mt_small))


mt_idio=cbind(mt_idio,mti_pc1=mt_pc_level[,1])
df[,colnames(mt_idio)]=NA
df[row.names(mt_idio),colnames(mt_idio)]=mt_idio
rm(mt_idio,mt_loadings,mt_pc_level,mt_small,PcMti)

# HinzufÃ¼gen der ifo r-indikatoren -----------------------------------------

ifo <- read.csv(paste(info$Sfile,'Daten/Alternative Indikatoren/ifo/r16.csv',sep=''), header=T,row.names=1)

row.names(ifo)=gsub('15/','',row.names(ifo))
rn.split=strsplit(row.names(ifo),'/')
rn.new=sapply(rn.split,function(x) paste(x[2],x[1],sep='/'))
row.names(ifo)=rn.new
rm(rn.new,rn.split)


df[,colnames(ifo)]=NA
df[row.names(ifo),colnames(ifo)]=ifo
rm(ifo)

# Hinzufuegen der R-word Indikatoren ---------------------------------------

rword=read.csv(paste(info$Sfile,'Daten/R-word Index/RWortindex_1986m1-2014m4.csv',sep=''), header=T)

rword$Year_Month=gsub('-','/',rword$Year_Month)
rword=rword[169:nrow(rword),]
add.zero=function(x){
        if(nchar(x)==1){x=paste(0,x,sep='')}
        return(x)
}
row.names(rword)=paste(rword$Year,sapply(rword$Month,add.zero),sep='/')
rword=rword[,grep('RWort',colnames(rword)),drop=F]

df[,colnames(rword)]=NA
df[row.names(rword),colnames(rword)]=rword

rm(rword)

# current account (RealeHandelsbilanz runterladen.R) ----------------------
CA=read.csv("I:/Personal_Folders/employees/KX/A_MTI/Daten/Bundesbank/Handelsbilanz/Handelsbilanz.csv",row.names=1)
df[,colnames(CA)]=NA
df[row.names(CA),colnames(CA)]=CA[row.names(CA),colnames(CA)]
rm(CA)

# employment  -------------------------------------------------------------
empl <- read.csv(paste(info$Sfile,'Daten/Alternative Indikatoren/employ.csv',sep=''), header=T,row.names=1,skip=1)
row.names(empl)=gsub('15/','',row.names(empl))
rn.split=strsplit(row.names(empl),'/')
rn.new=sapply(rn.split,function(x) paste(x[2],x[1],sep='/'))
row.names(empl)=rn.new
rm(rn.new,rn.split)
colnames(empl)='empl'
df$empl=NA
df[row.names(empl),colnames(empl)]=empl
rm(empl)

# yoy change rate ---------------------------------------------------------
period=12
tra_list=c("ip","dax","eur.stox","oil","manuf.order","usd","xnom","mnom","xreal","mreal",
           "xpi","mpi","tot","cpi",'CA','empl')

df[(period+1):nrow(df),tra_list]=((df[(period+1):nrow(df),tra_list]
                                              /df[1:(nrow(df)-period),tra_list])
                                              -1)
names(df)[grep('cpi',names(df))]='infl'
rm(tra_list)


rm(period,info)

# Adding zeit indicators --------------------------------------------------


zeit_all=read.csv(paste(DirCode,'/zeit.csv',sep=''),row.names=1)
zeit_economic=read.csv(paste(DirCode,'/zeit_economic.csv',sep=''),row.names=1)
colnames(zeit_economic)=paste(colnames(zeit_economic),'economic',sep='.')
zeit=zeit_economic
zeit[,colnames(zeit_all)]=NA
zeit[row.names(zeit_all),colnames(zeit_all)]=zeit_all
zeit=zeit[complete.cases(zeit),]
df[,colnames(zeit)]=NA
df[row.names(zeit),colnames(zeit)]=zeit
rm(zeit,zeit_all,zeit_economic)

# saving df to disk -------------------------------------------------------
write.csv(df,paste(DirCode,'/Data/data.csv',sep=''))
