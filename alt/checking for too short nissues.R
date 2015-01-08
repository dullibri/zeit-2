maxr=function(year){
        aux=register[register$year==year,]
        issueU=unique(aux$issue)
        max(issueU)
}
years=unique(register$year)
erg=sapply(years,maxr)
names(erg)=years
