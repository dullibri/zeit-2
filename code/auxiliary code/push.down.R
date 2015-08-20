push.down=function(variable,set){
        # pushes all variables to the forecast origin so that data with publication
        # lag may be used nontheless for forecasting.
        aux=set[,variable]
        naux=length(aux)
        values=aux[is.na(aux)==F]
        nvalues=length(values)
        aux2=rep(NA,naux)
        aux2[(naux-nvalues+1):naux]=values
        return(aux2)
}