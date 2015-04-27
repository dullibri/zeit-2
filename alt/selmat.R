selmat<-function(nx,max=nx){
  # selmat gibt die index matrix aller kombinationen von nx exogenen variablen und
  # einer endogenen mit einer gesamtanzahl von variablen max aus - Außer dem Fall, 
  # in dem nur die Endogene auftaucht.
  
  smat<-cbind(matrix(combn(1:nx,1),nx,1),matrix(-1,nx,max-1))
  
  
  for (i in 2:max){
    aux<-matrix(t(combn(1:nx,i)),ncol=i)
    if (i<max){aux<-cbind(aux,matrix(-1,nrow=nrow(aux),ncol=max-i))}
    
    smat<-rbind(smat,aux)
  }
  return(smat)
}