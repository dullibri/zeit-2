<<<<<<< HEAD
browseVignettes(package = 'qdap')
data(amplification.words)

pos=as.character(valueword['wert'>0,'wort',drop=T])
# pos=tolower(pos)
pos.w=valueword['wert'>0,'wert']
neg=as.character(valueword[valueword$wert<0,'wort',drop=T])
# neg=tolower(neg)
neg.w=valueword[valueword$wert<0,'wert']
pf=polarity_frame(pos,neg,pos.w,neg.w)

# pf=polarity_frame(pos,neg,pos.w,neg.w,env=F)

=======
require('qdap')
>>>>>>> 8e041f60150014cdea184b243283451685b834dd
# um das strip programm so  zu aendern, dass keine tolower mehr st --------

strip=function (x, char.keep = "~~", digit.remove = TRUE, apostrophe.remove = TRUE, 
                lower.case = TRUE) 
{
        strp <- function(x, digit.remove, apostrophe.remove, char.keep, 
                         lower.case) {
                if (!is.null(char.keep)) {
                        x2 <- Trim(gsub(paste0(".*?($|'|", paste(paste0("\\", 
                                                                        char.keep), collapse = "|"), "|[^[:punct:]]).*?"), 
                                        "\\1", as.character(x)))
                }
                else {
                        x2 <- Trim(gsub(".*?($|'|[^[:punct:]]).*?", "\\1", 
                                        as.character(x)))
                }
                if (lower.case) {
                        x2 <- x2
                }
                if (apostrophe.remove) {
                        x2 <- gsub("'", "", x2)
                }
                ifelse(digit.remove == TRUE, gsub("[[:digit:]]", "", 
                                                  x2), x2)
        }
        x <- clean(x)
        unlist(lapply(x, function(x) Trim(strp(x = x, digit.remove = digit.remove, 
                                               apostrophe.remove = apostrophe.remove, char.keep = char.keep, 
                                               lower.case = lower.case))))
}
assignInNamespace('strip',strip,'qdap')
<<<<<<< HEAD
# splittet den text in sätzte auf und fügt $tot hinzu zur paragraph zuordnung
test=sentSplit(df,"Text")
tp=polarity(df$'Text',polarity.frame=pf)
=======

DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2'
DirCode='h://Git/zeit-2'

# getting automated sentiment
# load valueword (a vector of SentiWS words in the first column, values in the second, 
# ambigous duplicates are eliminated, capitalizations are preserved)
valueword=read.csv(paste(DirCode,'/valueword.csv',sep=''))

pos=as.character(valueword['wert'>0,'wort',drop=T])
# pos=tolower(pos)
pos.w=valueword['wert'>0,'wert']
neg=as.character(valueword[valueword$wert<0,'wort',drop=T])
# neg=tolower(neg)
neg.w=valueword[valueword$wert<0,'wert']
pf=sentiment_frame(pos,neg,pos.w,neg.w)
# getting list of negative words
negating=read.csv(paste(DirCode,'/data/sentistrength_de/negators.csv',sep=''),header=F)
negating=negating[!negating=='']
negating=c(negating,c(''))
# splittet den text in sätzte auf und fügt $tot hinzu zur paragraph zuordnung
test=sentSplit(df,"Text")
tp=polarity(df$'Text',polarity.frame=pf,negators=negating)
>>>>>>> 8e041f60150014cdea184b243283451685b834dd

# tp=polarity(test,polarity.frame=pf)
tpa=tp$all

