DirCode='h:/Git/zeit-2/data/mediatenor/'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data/mediatenor/'
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
infl.de=df$Volume*df$de
monetary=df$mon
monetary.de=df$mon*df$de
all.de=df$de
all=all.de
all[1:length(all)]=T
infl.de.future=df$Volume*df$de*df$fut
tax.de.future=df$tax*df$de*df$fut
lab.de.future=df$lab*df$de*df$fut
cyc.de.future=df$cyc*df$de*df$fut
bud.de.future=df$bud*df$de*df$fut
quality.de=df$quality*df$de
# applying the indeces
indeces.new=data.frame(
        #         MT_all=index(all)
        MT_all_de=index(all.de)
        #         ,MT_monetary=index(monetary)
        ,MT_monetary_de=index(monetary.de)
        #         ,MT_inflation=index(infl)
        ,MT_inflation_de=index(infl.de)
        
        #         ,MT_all_climate=climate(all)
        ,MT_all_de_climate=climate(all.de)
        #         ,MT_monetary_climate=climate(monetary)
        ,MT_monetary_de_climate=climate(monetary.de)
        #         ,MT_inflation_climate=climate(infl)
        ,MT_inflation_de_climate=climate(infl.de)
        ,MT_quality_de=index(quality.de)
        ,MT_quality_de_climate=climate(quality.de)
#         ,MT_inflation_de_future=index(infl.de.future)
#         ,MT_taxation_de_future=index(tax.de.future)
#         ,MT_labor_de_future=index(lab.de.future)
#         ,MT_cycle_de_future=index(cyc.de.future)
#         ,MT_budget_de_future=index(bud.de.future)
        
)



indeces=cbind(indeces.lamla,indeces.new)
colnames(indeces)=gsub('_de','',colnames(indeces))
row.names(indeces)=dates

write.csv(indeces,paste(DirCode,'Mediatenor_Jan2001_Jan2015.csv',sep=''))

t=ts(indeces.lamla,start=c(2001,1),freq=12)
plot(t)

t=ts(indeces.new,start=c(2001,1),freq=12)
plot(t)
