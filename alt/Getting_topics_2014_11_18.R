
# getting complete register -----------------------------------------------

for (year in 2005:2014){
# year=2014
input<-paste('http://www.zeit.de/',year,'/index/seite-3',sep='')
plainhtml=readLines(input,encoding='UTF-8')
unlink(input)

fnissue<-function(year,webpage){
        # looks up the number of issues of one 'year' (mostly taking the value of 52 or 53)
        # 'webpage' is the third page of the index page of the respective year (plaintext)
        all_issue=matrix(NA,nrow=10,ncol=2)
        for (i in 45:54){
                index=grep(paste('http://www.zeit.de/',year,'/',i,'/index',sep=''),webpage)
                all_issue[i-45+1,]=c(i,length(webpage[index]))
        }
        min(all_issue[all_issue[,2]==0,1])-1
}
Nissue=fnissue(year,plainhtml)

registry<-function(year,issue){
        # makes a register of an issue in a year.
        # for each article it returns the weblink, its title, description, year, 
        # and issue
        issue_formated=as.character(issue)
        if (issue<10){
                issue_formated=paste(0,issue,sep='')
        }
        input <-paste('http://www.zeit.de/',year,'/',issue_formated,'/index',sep='')
        if (url.exists(input)==F){return(NULL)}
        plainhtml_all=readLines(input,encoding='UTF-8')
        unlink(input)
        
        # Getting ressorts and indeces, cutting off first part --------------------
        ressort_index=grep('<li class="archiveressort">',plainhtml_all)
        plainhtml=plainhtml_all[-c(1:(ressort_index[1]-1))]
        ressort_start=ressort_index-(ressort_index[1]-1)
        ressort_end=c(ressort_start[-1],length(plainhtml))-1
        ressorts=plainhtml[ressort_start]
        ressorts=gsub('<li class=\"archiveressort\">|</li>','',ressorts)
        dfressorts=data.frame(ressorts,ressort_start,ressort_end,length=ressort_end-ressort_start+1)
        mainressorts=paste('[Ww]irtschaft','[Pp]olitik','[Dd]ossier',sep='|')
        fmainind <- function(dfressorts,mainressorts){
                mdfressorts<-dfressorts[grep(mainressorts,dfressorts$ressorts),]
                Nmressorts<-nrow(mdfressorts)
                ind=matrix(NA,nrow=sum(mdfressorts[,'length']),ncol=1)
                j=1
                for (i in 1:Nmressorts){
                        
                        ind[j:(j-1+mdfressorts[i,4])]=mdfressorts[i,2]:mdfressorts[i,3]
                        j=(j-1+mdfressorts[i,4])+1
                }        
                return(ind)
        }
        ind=fmainind(dfressorts,mainressorts)
        plainhtml=plainhtml[ind]
        
        links_index=regexpr(paste('http://www.zeit.de/','(.*)','/','(.*)" tit',sep=''),plainhtml)
        links_raw=regmatches(plainhtml,links_index)
        links=gsub('" tit','',links_raw)
#         links_last=sapply(strsplit(links,'/'),function(x)x[6])
#         links_int=as.integer(links_last)
#         links=links[is.na(links_int)==T]
#         links=links[-grep('.xml',links)]
        if (length(links)==0){return(NULL)}
        
        plainhtml_redux=plainhtml[links_index!=-1]
        
        titles=matrix(NA,nrow=length(links),ncol=1)
        titles_index=regexpr('<h4 class="title\"(.*)</h4',plainhtml)
        titles_raw=regmatches(plainhtml,titles_index)
        titles_exist=gsub('<h4 class=\"title\">|</h4','',titles_raw)
        titles=titles_exist

        
        if (length(links)!=length(titles)){
                register=data.frame(link=links
                                    ,title=NA
                                    ,year=year
                                    ,issue=issue)   
        }else{
                register=data.frame(link=links
                                    ,title=titles
                                    ,year=year
                                    ,issue=issue) 
        }
        return(register)
}
for (issue in 1:Nissue){
        if (!'register'%in%ls()){
                register=registry(year,issue)
        }
        register=rbind(register
                       ,registry(year,issue))
}
}



# deriving topics from title ----------------------------------------------
library(tm)
library(SnowballC)
library(RCurl)
library(XML)
library(kernlab)
# test=paste(register$title,collapse=' ')
test=register$description
test=as.character(test)
test=gsub('-',' ',test)

# test=sapply(strsplit(register$description[1]))
# test=paste(register$title,collapse=' ')
# test=VectorSource(register$title)
test=VectorSource(test)
test=Corpus(test, readerControl = list(reader = readPlain,
                                       language="de"))

test <- tm_map(test, tolower)
test <- tm_map(test, removeNumbers)
test <- tm_map(test, removePunctuation)
test <- tm_map(test, function(x){removeWords(x,
                                             c("dass",stopwords("german")))})
test <- tm_map(test,function(x){stemDocument(x,language = "german")})

dtmtest=DocumentTermMatrix(test)
tt=findFreqTerms(dtmtest,20,Inf)
Mtest=as.matrix(dtmtest)
kurz<-Mtest[which(rownames(Mtest)%in%findFreqTerms(dtmtest,2,Inf)),]
findAssocs(dtmtest, c("wirtschaft"), c(0.1))
