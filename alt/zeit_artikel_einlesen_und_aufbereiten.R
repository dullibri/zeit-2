# 
# htmlToText <- function(input, ...) {
#   ###---PACKAGES ---###
#   require(RCurl)
#   require(XML)
#   ###--- LOCAL FUNCTIONS ---###
#   # Determine how to grab html for a single input element
#   evaluate_input <- function(input) {
#     # if input is a .html file
#     if(file.exists(input)) {
#       char.vec <- readLines(input, warn = FALSE)
#       return(paste(char.vec, collapse = ""))
#     }
#     # if input is html text
#     if(grepl("</html>", input, fixed = TRUE)) return(input)
#     # if input is a URL, probably should use a regex here instead?
#     if(!grepl(" ", input)) {
#       # downolad SSL certificate in case of https problem
#       if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
#       return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
#     }
#     # return NULL if none of the conditions above apply
#     return(NULL)
#   }
#   # convert HTML to plain text
#   convert_html_to_text <- function(html) {
#     doc <- htmlParse(html, asText = TRUE)
#     text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
#     return(text)
#   }
#   # format text vector into one character string
#   collapse_text <- function(txt) {
#     return(paste(txt, collapse = " "))
#   }
#   ###--- MAIN ---###
#   # STEP 1: Evaluate input
#   html.list <- lapply(input, evaluate_input)
#   # STEP 2: Extract text from HTML
#   text.list <- lapply(html.list, convert_html_to_text)
#   # STEP 3: Return text
#   text.vector <- sapply(text.list, collapse_text)
#   return(text.vector)
# }# load packages
# library(RCurl)
# library(XML)
# library(tm)
# # assign input (could be a html file, a URL, html text, or some combination of all three is the form of a vector)
# input <- 'http://www.zeit.de/2014/01/tuerkei-erdogan-korruption'
# plainhtml=readLines(input,encoding='UTF-8')
# unlink(input)
# # evaluate input and convert to text
# txt <- htmlToText(input)
# txt<-VectorSource(txt)
# x<-Corpus(txt,readerControl = list(reader = readPlain,language="de"))
# x <- tm_map(x, stripWhitespace)
# x <- tm_map(x, removeNumbers)
# x <- tm_map(x, removePunctuation)
# x <- tm_map(x, tolower)
# x <- tm_map(x, function(x){removeWords(x,
#                                                    c("dass",stopwords("german")))})
# 
# write.csv(plainhtml,'plainhtml.txt')

# weitere seiten desselben textes finden
# einseitiges Beispiel: http://www.zeit.de/2014/01/journalisten-politiker-reisen
# input <- 'http://www.zeit.de/2014/01/journalisten-politiker-reisen' # 1 seite
# input <- 'http://www.zeit.de/2014/01/tuerkei-erdogan-korruption'# 2 seiten
input <-'http://www.zeit.de/2014/01/kinderarbeit-bolivien'# 3 seiten
fnextpages<-function(plainhtml,index){
  plainhtml_index=plainhtml[index]
  plainhtml_index_split=unlist(strsplit(plainhtml_index,'"'))
  links=plainhtml_index_split[grep('http',plainhtml_index_split)]
  unique(links) 
}
convert_html_to_text <- function(html) {
  # extracted from: convert_html_to_text <- function(html) {
  doc <- htmlParse(html, asText = TRUE)
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  return(text)
}

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
# 
# # html code entfernen
# html_cleaner<-function(mittelteil){
#   
# }
# txt<-VectorSource(mittelteil)
# x<-Corpus(txt,readerControl = list(reader = readPlain,language="de"))
# x <- tm_map(x, stripWhitespace)
# x <- tm_map(x, removeNumbers)
# x <- tm_map(x, removePunctuation)
# x <- tm_map(x, tolower)
# x <- tm_map(x, function(x){removeWords(x,
#                                        c("dass",stopwords("german")))})
# 
# 
# test=convert_html_to_text(mittelteil)
mittelteil=mittelteil[mittelteil!="\n"]
mittelteil=mittelteil[mittelteil!="\n"]
mittelteil=gsub('\n','',mittelteil,)
mittelteil=paste(mittelteil,collapse='')
write.csv(mittelteil,'test.txt')

