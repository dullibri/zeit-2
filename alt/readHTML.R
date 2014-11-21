readHTML<-function(elem, language, id){
  # aufruf: html2txt  filename.html   filename.txt
  # quelle: http://www.u32.de/soft_htm.html
  content <- system2("html2text", shQuote(elem$uri), stdout = TRUE)
  PlainTextDocument(content, id = id, language = language,
                    origin=sub("/.*", "", sub(".*Publikationen/*", "", elem$uri)))
}