lag<-function(ser,nlag){
  # computes nlag lags of time series(matrix) ser 
  # nlag is the number of lags
  # ncol is the number of variables stacked on top of each other
  # returns original ser of the first variable and lags for the window, 
  # for which all data are complete of the first (endogenous) and exogenous
  # (1st number of observation will be nlag+1)
  # outl is without endogenous original observations
  # out is with exogneous original data.
  if (is.matrix(ser)==T||is.data.frame(ser)==T){
    nobs=nrow(ser)
    nv=ncol(ser)
    }else{
      nobs=length(ser)
    nv=1
    }
  
  
  ser=unlist(matrix(ser))
  ind<-matrix(rep((nlag+1):nobs,nlag+1),(nobs-nlag),nlag+1)-matrix(rep(0:nlag,nobs-nlag),nobs-nlag,nlag+1,byrow=TRUE)
  if (nv>1){
    for (i in 2:nv) {
      ind<-cbind(ind,ind[,1:(nlag+1)]+nobs*(i-1))
    }
  }
  
  out<-matrix(ser[c(ind)],ncol=(nlag+1)*nv)
  outl<-out[,-1]
  outp<-out[,-ncol(out)]
  
  if (nv>1){
    ind2<-seq(2+nlag,nv*(nlag+1),nlag+1)
    ind2<-cbind(1,ind2)
    ind3<-seq(1+nlag,nv*(nlag+1),nlag+1)
    outl<-out[,-ind2]
    outp<-out[,-ind3]
    
  }
  res<-list(outl,out,outp)
  return(res)
}