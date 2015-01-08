dfor<-function(data,h=1,autoreg=1,nlag=1){
  # Erstellt direkte und ADL Prognosen. Alle endogenen und exogenen Regressoren
  # gehen mit der selben Anzahl Lags ein. Die Lags werden mit BIC optimiert.
  # data ist eine matrix mit der zu erkl?renden in der ersten, den erkl?renden in den folgenden spalten
  # nlag <- ist die maximale anzahl auf lags die mit hilfe des BIC gepr?ft werden sollen.
  # h ist der Prognosehorizont
  # autoreg == 1 bedeutet, dass die endogene als Regressor aufgenommen wird.

  # braucht: 
  # lag.R

# Bearbeitungsschritte ----------------------------------------------------
# 15.05.2014: FEHLVERSUCH: lm durch lm.fit ersetzen um Effizienz zu steigern. BIC NUTZT lm.fit nicht
# 09.05.2014: lag-selection auf die finiten bic-werte, beschränkt.
# 08.05.2014: lag-selection über apply.
# 08.05.2014: gibt nur noch die prognose aus. optlag wird nicht ausgegeben
# 05.05.2014: dfor aus schleif gebaut. 

# autoregressives modell
if (is.data.frame(data)==0){
  data=matrix(c(data,data),ncol=2)
  nv=2
  autoreg=0 # das ist wegen der Berechnung weiter unten.
}
if (is.data.frame(data)==1){nv<-length(data)}
# output matrix anlegen, data als Matrix formatierenoutput matrix anlegen, data als Matrix formatieren ------------------------------------------------------------------------

out<-list()
data=as.matrix(data)

erg.lm<-list()
X<-matrix()
Y<-matrix()

Nobs=nrow(data)
lag_sel<-function(i,data,Nobs,h,autoreg,nv){
  X<-matrix(lag(data[1:(Nobs-h+1),(2-autoreg):(nv)],i)[[1]]
            ,ncol=(nv+autoreg-1)*i)
  X<-X[(1+(nlag-i)):nrow(X),] # braucht man hier, damit alle X der verschiedenen lags gleich viele observationen haben
  Y<-data[(h+nlag):Nobs,1,drop=F] 
  out<-BIC(lm(Y~X))
  return(out)
}
bic<-matrix(apply(matrix(1:nlag,nlag,1),1,lag_sel,data,Nobs,h,autoreg,nv),nlag,1)


erg.lm<-lm(data[(which.min(bic[is.finite(bic)])+h):(Nobs),1,drop=F]~
                 lag(data[1:(Nobs-h+1),(2-autoreg):(nv)],which.min(bic[is.finite(bic)]))[[1]])
out<-sum(erg.lm$coeff*c(1,tail(lag(data[1:(Nobs-h+1),(2-autoreg):(nv)],which.min(bic[is.finite(bic)]))[[3]],1) ))
# out$lag<-which.min(bic)      

return(out)
}