# Verbesserung: exp, exp.pr, im und im.pr aus CA Datei. Empl direkt aus alt_ind - up-daten

# notwendig, um den pfad zu finden.
info<-list()
info$Sfile='I:/Personal_Folders/employees/KX/A_MTI/'
info$SfileCode='I:/Personal_Folders/employees/KX/A_MTI/Code/'
info$SfileData='I:/Personal_Folders/employees/KX/A_MTI/Daten/MF/'
outdirfile='h:/git/zeit-2/nonZeitindicators.csv'

# Alternative Indikatoren laden -------------------------------------------------------------

alt_ind <- read.csv(paste(info$Sfile,'Daten/Alternative Indikatoren/alternative_indicators_long.csv',sep='')
                ,row.names=1 
                ,header=T)

dat=alt_ind
dat$spread=dat$long.rate-dat$short.rate
rm(alt_ind)

# renaming row.names ------------------------------------------------------
row.names(dat)=gsub('-15','',row.names(dat))
rn.split=strsplit(row.names(dat),'-')
rn.new=sapply(rn.split,function(x) paste(x[1],x[2],sep='/'))
row.names(dat)=rn.new
rm(rn.new,rn.split)
# Einzelne Variablen rausnehmen -------------------------------------------

dat$m0=NULL
dat$m1=NULL
dat$m2=NULL
dat$m3=NULL
dat$gold=NULL
dat$eer=NULL
# dat$u=NULL
dat$constr.order=NULL
dat$prod=NULL
dat$wholesale=NULL
dat$retail.sales=NULL
dat$bus.exp=NULL

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

dat[,colnames(mt_small)]=NA
dat[row.names(mt_small),colnames(mt_small)]=mt_small
dat$MTI_Past=NULL # NAs


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
dat[,colnames(mt_idio)]=NA
dat[row.names(mt_idio),colnames(mt_idio)]=mt_idio
rm(mt_idio,mt_loadings,mt_pc_level,mt_small,PcMti)

# HinzufÃ¼gen der ifo r-indikatoren -----------------------------------------

ifo <- read.csv(paste(info$Sfile,'Daten/Alternative Indikatoren/ifo/r16.csv',sep=''), header=T,row.names=1)

row.names(ifo)=gsub('15/','',row.names(ifo))
rn.split=strsplit(row.names(ifo),'/')
rn.new=sapply(rn.split,function(x) paste(x[2],x[1],sep='/'))
row.names(ifo)=rn.new
rm(rn.new,rn.split)


dat[,colnames(ifo)]=NA
dat[row.names(ifo),colnames(ifo)]=ifo
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

dat[,colnames(rword)]=NA
dat[row.names(rword),colnames(rword)]=rword

rm(rword)

# current account (RealeHandelsbilanz runterladen.R) ----------------------
CA=read.csv("I:/Personal_Folders/employees/KX/A_MTI/Daten/Bundesbank/Handelsbilanz/Handelsbilanz.csv",row.names=1)
dat$CA=NA
dat[row.names(CA),'CA']=CA[row.names(CA),'CA']
rm(CA)

# employment  -------------------------------------------------------------
empl <- read.csv(paste(info$Sfile,'Daten/Alternative Indikatoren/employ.csv',sep=''), header=T,row.names=1,skip=1)
row.names(empl)=gsub('15/','',row.names(empl))
rn.split=strsplit(row.names(empl),'/')
rn.new=sapply(rn.split,function(x) paste(x[2],x[1],sep='/'))
row.names(empl)=rn.new
rm(rn.new,rn.split)
colnames(empl)='empl'
dat$empl=NA
dat[row.names(empl),colnames(empl)]=empl
rm(empl)

# yoy change rate ---------------------------------------------------------
period=12
tra_list=c("ip","dax","eur.stox","oil","manuf.order","usd","ex","im",
           "ex.pr","im.pr","tot","cpi",'CA','trade.bal','empl')

dat[(period+1):nrow(dat),tra_list]=((dat[(period+1):nrow(dat),tra_list]
                                              /dat[1:(nrow(dat)-period),tra_list])
                                              -1)
names(dat)[grep('cpi',names(dat))]='infl'
rm(tra_list)


rm(period,info)

write.csv(dat,outdirfile)
