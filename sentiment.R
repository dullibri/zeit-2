function (text, valueword) 
{
    if (length(text) == 2 & text[1] == ",x") {
        text = text[2]
    }
    text = gsub("[[:punct:]]", "", text)
    text.split = sapply(strsplit(text, " "), function(x) x)
    ind = valueword[, 1] %in% text.split
    valdf = valueword[ind, , drop = F]
    valdf$h = sapply(valueword[ind, 1], function(x) sum(text.split %in% 
        x))
    nwords = length(text.split)
    return(list(valdf, nwords))
}
