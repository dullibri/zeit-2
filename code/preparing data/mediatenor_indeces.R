# DirCode='h:/Git/zeit-2/data/mediatenor/'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data/mediatenor/'
DirOut='C:/Users/Dirk/Documents/github/zeit-2/'
df=read.csv(paste(DirCode,'WiLage_lang_2001m01-2015m01_MTI.csv',sep=''))

# preparing dates
df$my=gsub('^[0-9]{2}\\.','',df$Datum)
t=t(sapply(strsplit(df$my,'\\.'),function(x)x))
tym=paste(t[,2],t[,1],sep='-')
df$ym=tym

# monetary issues, 
df$mon=df$Themengruppe=='Währungspolitik/EURO/Geldpolitik'
df$tax=df$Themengruppe=='Haushaltspolitik'
df$lab=df$Themengruppe=='Arbeitsmarkt'
df$cyc=df$Themengruppe=='Konjunktur'
df$bud=df$Themengruppe=='Währungspolitik/EURO/Geldpolitik'

df$quality=df$Medium!='Bild-Zeitung'
# time reference
df$fut=df$Zeitbezug=='Zukunft'
df$pre=df$Zeitbezug=='Gegenwart'

# polarity 
df$positive=df$Bewertung=='positiv'
df$negative=df$Bewertung=='negativ'
df$neutral=df$Bewertung=='ohne eindeutige Wertung'

# identifying news with reference to Germany
t=unique(df$Raumbezug)
t=as.character(t[grep('eutsch',t)])
df$de=df$Raumbezug%in%t


# lamla lein indices ------------------------------------------------------
df$Notrend=df$Thema=='Preisindices (z.B. Inflationsrate), allgemein'
df$Rising=df$Thema=='Steigendes Inflation oder hohes Niveau'
df$Falling=df$Thema=='Sinkende Inflation oder niedriges Niveau'
df$Volume=df$Rising+df$Falling+df$Notrend

df$Goodrising=df$pos==T&df$Thema=='Steigendes Inflation oder hohes Niveau'
df$Badrising=df$neg==T&df$Thema=='Steigendes Inflation oder hohes Niveau'
df$Otherrising=df$neutral==T&df$Thema=='Steigendes Inflation oder hohes Niveau'

lamla.names=c('Notrend','Rising','Falling','Volume','Goodrising','Badrising','Otherrising')

# creating lamla-style indexes based on a subsample only counting values with reference to Germany
df.subsample=df[,c(lamla.names)]*df$de
indeces.lamla=aggregate(df.subsample,list(df$ym),sum)
dates=indeces.lamla$Group.1
indeces.lamla$Group.1=NULL

# export some descriptives (also only Germany related) ------------------------------------------------
df.lamla=df[df$de==1,c('Volume','Notrend','Falling','Rising','Goodrising','Badrising','Otherrising')]
df.lamla.exp=data.frame(Counts=colSums(df.lamla))
df.lamla.exp$Percentage=df.lamla.exp[,1]/df.lamla.exp[1,1]*100
writeLines(stargazer(df.lamla.exp,summary=F,title='Number of news items for each class'),paste(DirOut,'/tables/lamla.tex',sep=''))


# creating functions to built ifo-like indeces and climate
index=function(sel){
        t=cbind(ym=df$ym,df[,c('positive','negative','neutral')]*sel)
        ta=aggregate(t[,2:4],list(t$ym),sum)
        ind=(ta$positive-ta$negative)/(ta$positive+ta$negative+ta$neutral)
        ind[is.na(ind)==T]=0
        return(ind)
}
climate=function(sel){
        fut.ind=index(sel*df$fut)
        pre.ind=index(sel*df$pre)
        clim=((fut.ind+100)*(pre.ind+100))^.5
        clim[is.na(clim)==T]=0
        return(clim)
}

# selecting subindeces
infl=df$Volume

all.de=df$de
present.de=df$de*df$pre
future.de=df$de*df$fut
monetary.de=df$mon*df$de
tax.de=df$tax*df$de
infl.de=df$Volume*df$de
cyc.de=df$cyc*df$de
lab.de=df$lab*df$de
bud.de=df$bud*df$de

monetary=df$mon
all=all.de
all[1:length(all)]=T
infl.de.future=df$Volume*df$de*df$fut
tax.de.future=df$tax*df$de*df$fut
lab.de.future=df$lab*df$de*df$fut
cyc.de.future=df$cyc*df$de*df$fut
bud.de.future=df$bud*df$de*df$fut
quality.de=df$quality*df$de


# Exporting some descriptives of ifo like Mediatenor indices --------------

df.mti=df[df$de==1,c('pre','fut','Volume','mon','tax','lab','cyc','bud')]
df.exp=data.frame(c(Total=nrow(df.mti),colSums(df.mti)))
row.names(df.exp)=c('Total','Present','Future','Inflation','Monetary','Taxation','Labor','Cycle','Budget')
df.exp[,2]=df.exp/df.exp['Total',1]*100
colnames(df.exp)=c('Count','Percentage')
writeLines(stargazer(df.exp,summary=F,title='Number of news items for each class'),paste(DirOut,'/tables/MTI number of news items.tex',sep=''))

# applying the indeces
indeces.new=data.frame(
        MTI_All=index(all.de)
        ,MTI_Present=index(present.de)
        ,MTI_Future=index(future.de)
        ,MTI_Monetary=index(monetary.de)
        ,MTI_Inflation=index(infl.de)
        ,MTI_Taxation=index(tax.de)
        ,MTI_Cycle=index(cyc.de)
        ,MTI_Labor=index(lab.de)
        ,MTI_Budget=index(bud.de)               
)



indeces=cbind(indeces.lamla,indeces.new*100)
colnames(indeces)=gsub('_de','',colnames(indeces))
row.names(indeces)=dates

write.csv(indeces,paste(DirCode,'Mediatenor_Jan2001_Jan2015.csv',sep=''))

t=ts(indeces.lamla,start=c(2001,1),freq=12)
plot(t)

t=ts(indeces.new,start=c(2001,1),freq=12)
plot(t)
