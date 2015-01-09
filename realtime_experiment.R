# Bearbeitungsschritte ----------------------------------------------------



max=1 # maximum number of variables additional to endogenous
frec=42 # minium number of observations.
rolling_window=0 # if "0" no rolling window implemented ("1" else)
nlag=12 # number of lags to be tested.

library(corrgram)
library(forecast)
library(vars)
DirCode='H:/git/zeit-2'
DirRes=paste(DirCode,'/Results',sep='')

source(paste(DirCode,'/nac.R',sep=''))
source(paste(DirCode,'/lag.R',sep=''))
source(paste(DirCode,'/dfor.R',sep=''))
source(paste(DirCode,'/selmat.R',sep=''))
source(paste(DirCode,'/rsort.R',sep=''))

experiment=function(target){
        
        # target ='eur.stox'
        
        # read data ---------------------------------------------
        df=read.csv(paste(DirCode,'/Data/data.csv',sep=''),row.names=1)
        
        df=df[grep('2001/01',row.names(df)):grep('2014/02',row.names(df)),]
        sy=which(colnames(df)==target) # spalte, in der sich die endogene befindet.
        
        
        # endogene nach vorne bringen ---------------------------------------------
        
        df<-cbind(df[,c(sy),drop=F],df[,-sy])# ,bringt die endogene und die nebenan gelegene variable nach vorne.
        rm(sy)
        
        nx=ncol(df)-1 # Anzahl der Exogenen muss angepasst werden
        # subsample fürs testen ---------------------------------------------------
        # 
        # df=df[,1:10]
        # nx=ncol(df)-1
        
        # Selektionsmatrix und Ergebnissmatrix anlegen ------------------------------------------------
        if (max==1){
                sl<-cbind(1,matrix(1:nx,ncol=1)+1)
        }else{
                sl<-selmat(nx,max)
                sl<-sl+1 # +1 damit alle kombinationen ohne die endogene aufgelistet sind
                sl<-cbind(1,sl) # stellt die endogene in die erste spalte
        }
        sl<-rbind(c(1,rep(0,ncol(sl)-1)),sl)
                
        #  ------------------------------------------------------------------------
        for (hlauf in c(1,3,6,12)){ # hlauf=6
                print(paste('first forcast origin: ',row.names(df)[frec],sep=''))
                lrec=nrow(df) # 
                nrec=lrec-frec+1
                
                nmod=nrow(sl) # Anzahl der Modell
                forecasts=matrix(NA,nrec,nmod)
                colnames(forecasts)=colnames(df)
                rownames(forecasts)=paste(row.names(df)[frec:nrow(df)],'h:',hlauf,sep=' ')
                
                begin_test=proc.time()
                for (rec in frec:lrec){
                        #                   Rprof("boot.out")
                        #   rec=42
                        rn=rec-frec+1
                        df_ss=df[rn^rolling_window:rec,] # keine realtime data, dann muss auch das "reihe nach vorne schieben" weg
                        for (id_mod in 2:nmod){
                                forecasts[rn,id_mod]=predict(VAR(y=df_ss[,sl[id_mod,]],ic='AIC',lag.max=nlag),n.ahead=hlauf)$fcst[[1]][hlauf,1]
                                #                                 modloop(sl[id_mod,],df_ss=df_ss,h=hlauf,maxx=nlag)        
                        }
                        
                        forecasts[rn,1]=predict(ar(x=df_ss[,1],order.max=nlag),n.ahead=hlauf)$pred[hlauf]
                        print(rn)
                        #                   Rprof(NULL)
                        #                   summaryRprof('boot.out')
                        
                }
                
                end_test=proc.time()
                d=end_test-begin_test
                if (rolling_window==1){RW='_rolling_window'}else{RW=''}
                write.csv(forecasts,paste(DirRes,'/',target,'/',target,'_Horizon_',hlauf,'_VAR',RW,'_forecasts.csv',sep=''))
                save.image(paste(DirRes,'/',target,'/',target,'_Horizon_',hlauf,'_VAR',RW,'.RData',sep=''))
                
        }
}
targets=c('oil','eur.stox','usd','CA')
for (target in targets){
        dir.create(paste(DirCode,'/Results/',target,sep=''))
        experiment(target)}