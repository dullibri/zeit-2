DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
df=read.csv(paste(DirCode,'/data/mediatenor/WiLage_lang_2001m01-2015m01_MTI.csv',sep=''))

df$my=gsub('^[0-9]{2}\\.','',df$Datum)

df$Notrend=df$Thema=='Preisindices (z.B. Inflationsrate), allgemein'
df$cur=df$Themengruppe=='Währungspolitik/EURO/Geldpolitik'
df$fut=df$Zeitbezug=='Zukunft'
df$Rising=df$Thema=='Steigendes Inflation oder hohes Niveau'
df$Falling=df$Thema=='Sinkende Inflation oder niedriges Niveau'
df$Volume=df$Rising+df$Falling+df$Notrend
df$Goodrising=df$Bewertung=='positiv'&df$Thema=='Steigendes Inflation oder hohes Niveau'
df$Badrising=df$Bewertung=='negativ'&df$Thema=='Steigendes Inflation oder hohes Niveau'
df$Otherrising=df$Bewertung=='ohne eindeutige Wertung'&df$Thema=='Steigendes Inflation oder hohes Niveau'
df$positive=df$Volume==T&df$Bewertung=='positiv'
df$negative=df$Volume==T&df$Bewertung=='negativ'
df$neutral=df$Volume==T&df$Bewertung=='ohne eindeutige Wertung'


df$cur_positive=df$cur==T&df$Bewertung=='positiv'
df$cur_negative=df$cur==T&df$Bewertung=='negativ'
df$cur_neutral=df$cur==T&df$Bewertung=='ohne eindeutige Wertung'

t=unique(df$Raumbezug)
t=as.character(t[grep('eutsch',t)])
df$de=df$Raumbezug%in%t

ind.names=c('Notrend','Rising','Falling','Volume','Goodrising','Badrising','Otherrising'
            ,'positive','negative','neutral'
            ,'cur_positive','cur_negative','cur_neutral')
ind.de.names=paste(ind.names,'de',sep='_')
df[,ind.de.names]=df[,ind.names]*df$de
ind.names=c(ind.names,ind.de.names)
indeces=aggregate(df[,c(ind.names)],list(df$my),sum)
indeces$MT_inflation=100*(indeces$positive-indeces$negative)/(indeces$positive+indeces$negative+indeces$neutral)
indeces$MT_inflation_de=100*(indeces$positive_de-indeces$negative_de)/(indeces$positive_de+indeces$negative_de+indeces$neutral_de)
indeces$MT_currency=100*(indeces$cur_positive-indeces$cur_negative)/(indeces$cur_positive+indeces$cur_negative+indeces$cur_neutral)
indeces$MT_currency_de=100*(indeces$cur_positive_de-indeces$cur_negative_de)/(indeces$cur_positive_de+indeces$cur_negative_de+indeces$cur_neutral_de)

t=indeces$MT_inflation
indeces$MT_inflation[is.nan(t)]=0
t=indeces$MT_inflation_de
indeces$MT_inflation_de[is.nan(t)]=0
t=indeces$MT_currency
indeces$MT_currency[is.nan(t)]=0
t=indeces$MT_currency_de
indeces$MT_currency_de[is.nan(t)]=0



t=t(sapply(strsplit(indeces$Group.1,'\\.'),function(x)x))
tym=paste(t[,2],t[,1],sep='-')
indeces$ym=tym


indeces=indeces[sort(indeces$ym,index.return=T)[[2]],]
indeces$Group.1=NULL
row.names(indeces)=indeces$ym
indeces$ym=NULL


dif=indeces[,ind.names[1:7]]-indeces[,ind.de.names[1:7]]
dif=ts(dif,start=c(2001,1),freq=12)

fin.names=ind.names
fin.names=fin.names[-grep('_de|positive|negative|neutral',fin.names)]
fin.names=c(fin.names,'MT_inflation','MT_inflation_de','MT_currency','MT_currency_de')
export=indeces[,fin.names]
write.csv(export,paste(DirCode,'/data/mediatenor/lamlalein.csv',sep=''))
# plot(indeces$Volume,type='l')
# t=ts(indeces,start=c(2001,1),freq=12)
# plot(t$Volume)
# plot(t)
