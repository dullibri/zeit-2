rsort<-function(x){
  nr<-nrow(x)
  nc<-ncol(x)
for (i in 1:nr){
 x[i,1:nc]<-sort(x[i,1:nc])
}
  return(x)
}


