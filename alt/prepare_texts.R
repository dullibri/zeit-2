function(input,year,issue,title){
# input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 3 seiten


plainhtml=readLines(input,encoding='UTF-8')
unlink(input)

mittelteil=plainhtml[(grep('articleheader',plainhtml)+1):(grep('articlefooter',plainhtml)-1)]
mittelteil<-convert_html_to_text(mittelteil)
index=grep(paste(input,'/seite-',sep=''),plainhtml)
if (length(index)!=0){
weitere_seiten=fnextpages(plainhtml,index)
seiten_zahl=length(weitere_seiten)+1
for (i in 2:seiten_zahl){
  plainhtml=readLines(weitere_seiten[i-1],encoding='UTF-8')
  unlink(input)
  zusaetzlicher_text=plainhtml[(grep('articleheader',plainhtml)+1):(grep('articlefooter',plainhtml)-1)]
  zusaetzlicher_text<-convert_html_to_text(zusaetzlicher_text)
  mittelteil=c(mittelteil,zusaetzlicher_text)
  
}
}
mittelteil=mittelteil[mittelteil!="\n"]
mittelteil=mittelteil[mittelteil!="\n"]
mittelteil=gsub('\n','',mittelteil,)
mittelteil=paste(mittelteil,collapse='')
write.csv(mittelteil,paste(year,'-',issue,'-',title,'.txt'sep='')
}
