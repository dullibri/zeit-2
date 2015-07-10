for (h in 1:12){# h=1
        inc=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/includeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exc=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/excludeSQ',sfeorr,h,'.csv',sep=''),header=F)
        exin=rbind(exc,inc)
        
        inc.ind=rep(1,nrow(exin))
        inc.ind[1:nrow(exc)]=0
        pval=read.csv(paste(DirCode,'/results/fe_infl_rolling_aic/pvalsSQ',sfeorr,h,'.csv',sep=''),header=F)
        
        mcs.h=data.frame(exin,pval,inc.ind)
        sfe.exp=sfe[lin,]
        
        mods.sfe=row.names(sfe.exp)
        if (sfeorr=='sfe'){
                result=rs[[h]] 
                result=result[mods.sfe,]
        }else{
                result=cr[[h]]
                result=result[mods.sfe,]
        }
        
        result[,c('model.id','pval','inc')]=NA
        result[mcs.h[,1],c('model.id','pval','inc')]=mcs.h  
        row.names(result)[which(row.names(result)%in%old.names)]=new.names
        fres[[h]]=result
        
}
nmod=nrow(result)
ttt=sapply(fres,function(x){
        t1=x[,c('theilsu')]
        t1=format(round(t1, 2), nsmall = 2)
        t1=as.character(t1)
        
        
        t1[x[2:nmod,'cw.p']<=0.05]=paste(t1[x[2:nmod,'cw.p']<=0.05],'star',sep='')
        t1[x[2:nmod,'cw.p']<=0.01]=paste(t1[x[2:nmod,'cw.p']<=0.01],'star',sep='')
        IN=(x[2:nmod,'inc']==1&x[2:nmod,'cw.p']<=1)
        IN=c(1,IN)==1
        t1[!IN]=''
        # t1=c('1.00',t1)
        t1[x[,'theilsu']<1&IN==T]=paste('DAA',t1[x[,'theilsu']<1&IN==T],'DB',sep='')
        t2=data.frame(
                round(
                        rank(
                                x[,'rank.theilsu',drop=F]
                        )
                        ,0)
        )
        
        t2[!IN,1]=''
        t2[IN,1]=paste('(',t2[IN,],')',sep='')
        t1[IN]=paste(t1[IN],t2[IN,1],sep=' ')
        t1=data.frame(t1)
        #         t1=data.frame(t1,t2,stringsAsFactors = F)
        #         
        #         #         t2=round(x[,c('pval')],2)
        #         #         t2=as.character(t2)
        #         #         t2[x[,'inc']==1]=paste(t2[x[,'inc']==1],'+',sep='')
        #         t=data.frame(t1,t2)
        
        return(t1)
}
)
ttt=sapply(ttt,function(x)x)
row.names(ttt)=row.names(result)
names.tab=c('ar',media.mods[,2])
tab1=ttt[names.tab,]
row.names(tab1)[1]='AR'
# colnames(tab1)[seq(1,23,2)]=NA
colnames(tab1)=paste('horizon',1:12,sep='=')
tab1out=tab1[,c(1,3,6,12)]

allrank=sapply(fres,function(x)x$rank.theilsu)
allrank=apply(allrank,2,rank)
row.names(allrank)=row.names(result)
media.mods.incD=c(media.mods[,2],paste('D',media.mods[,2],sep=''))
allrank=allrank[!row.names(allrank)%in%media.mods.incD,]
allrank=allrank[,c(1,3,6,12)]
almin=apply(allrank,2,which.min)
alnames=row.names(allrank)[almin]
allrank=allrank[almin,]
di=paste('(',diag(allrank),')',sep='')
allrank[,]=''
diag(allrank)=di


allcwp=sapply(fres,function(x)x$cw.p)
row.names(allcwp)=row.names(result)
allcwp=allcwp[!row.names(allcwp)%in%media.mods.incD,]
allcwp=allcwp[almin,c(1,3,6,12)]
di=diag(allcwp)
allstars.di=sapply(di,function(x){
        if(x<0.05){ret='star'}
        if(x<0.01){ret='starstar'}
        if(x>0.05){ret=''}
        return(ret)
}
)
# allstars=allcwp
# allstars[,]=''
# diag(allstars)=allstars.di

allthu=sapply(fres,function(x)x$theilsu)
row.names(allthu)=row.names(result)
allthu=allthu[!row.names(allthu)%in%media.mods.incD,]
allthu=allthu[almin,c(1,3,6,12)]
di=diag(allthu)
diEM=di<=1

allthu[,]=''
diag(allthu)=round(di,2)
for (i in 1:4){
        if (diEM[i]==T){
                diag(allthu)[i]=paste('DAA',diag(allthu)[i],allstars.di[i],'DB',sep='')
                diag(allthu)[i]=paste(diag(allthu)[i],' ',sep='')
        }
}



altern=matrix(paste(allthu,allrank,sep=''),nrow=4)
row.names(altern)=alnames
tab1out=rbind(altern,tab1out[2:nrow(tab1out),])
row.names(tab1out)[10:24]=paste('MTI',row.names(tab1out),sep=' ')[10:24]

out=stargazer(tab1out,summary=F
              ,title = Title
              ,font.size ='scriptsize' 
              ,notes='The table shows the RMSE ratio of the 
              respective model and the autoregressive (AR) model,
              so that a value < 0 (marked with bold letters) 
              indicates dominance of the respective model over 
              the AR. ** (*) indicates 
              statistically significant outperformance 
              to the one (five) percent level based on the Clark-West test.'
)

out=gsub('DAA','\\\\textbf{',out)
out=gsub('DB','}',out)
out=gsub('starstar','$^{**}$',out)
out=gsub('star','$^{*}$',out)
out=gsub('ccccc','lcccc',out)