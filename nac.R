nac<-function(x){
  #löscht Reihen, die ausschließlich NA sind
  out<-matrix(NA,1,ncol(x))
  suj<-matrix(NA,1,ncol(x))
  for (i in 1:ncol(x)){
    suj[1,i]=sum(is.na(x[,i]))
    if (sum(is.na(x[,i])==TRUE)==nrow(x)){
      out[1,i]=1
    }
    
  }
  return(out)
}
