setwd('C:/Users/Dirk/zeit')
year=2014
issue=1
issue_formated=c(paste(rep(0,9),1:9,sep=''),paste(10:12,sep=''))


input <-paste('http://www.zeit.de/',year,'/',issue_formated[issue],'/index',sep='')
plainhtml=readLines(input,encoding='UTF-8')
unlink(input)
index=grep(paste('http://www.zeit.de/',year,'/',issue_formated[issue],sep=''),plainhtml)
register_raw=unique(plainhtml[index])
register_raw=register_raw[-grep(input,register_raw)]
t1=register_raw[1]

plainhtml_index_split=unlist(strsplit(register_raw,'"'))
links=plainhtml_index_split[grep('http',plainhtml_index_split)]
titles=plainhtml_index_split[grep('title=',plainhtml_index_split)+1]
descriptions=matrix(sapply(links,function(x){
  y=unlist(strsplit(x,'/'))
  y[length(y)]
}))

register=data.frame(link=unique(links)
                    ,title=titles
                    ,description=descriptions
                    ,year=year
                    ,issue=issue) 
