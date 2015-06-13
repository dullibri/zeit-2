mtoq=function(mdat){
        mdat[,c('y','m')]=matrix(
                as.numeric(
                        sapply(
                                sapply(row.names(mdat),strsplit,'-'),function(x)x))
                ,ncol=2,byrow=T)

        mdat$quarter=NA
        mdat$quarter[mdat[,'m']%in%c(1,2,3)]=1
        mdat$quarter[mdat[,'m']%in%c(4,5,6)]=2
        mdat$quarter[mdat[,'m']%in%c(7,8,9)]=3
        mdat$quarter[mdat[,'m']%in%c(10,11,12)]=4
        mdat$yq=paste(mdat[,'y'],mdat$quarter,sep='-')
        qdat=data.frame(yq=unique(mdat$yq))
        nvar=ncol(mdat)-4
        for (i in 1:nvar){
                Nna=aggregate(is.na(mdat[,i]),list(as.factor(mdat$yq)),sum)[,2]==0
                Nincomplete=aggregate(apply(mdat[,i,drop=F],1,is.numeric),list(as.factor(mdat$yq)),sum)[,2]==3
                OK=(Nna+Nincomplete)==2
                qdat[which(OK),colnames(mdat)[i]]=aggregate(mdat[,i,drop=F],list(as.factor(mdat$yq)),mean)[OK,2]
           
        }
        row.names(qdat)=qdat$yq
        qdat$yq=NULL
        
}
