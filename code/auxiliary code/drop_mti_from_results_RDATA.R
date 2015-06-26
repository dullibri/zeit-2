DirCode='h:/Git/zeit-2/results/'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/results/'
for (i in 1:12){#i=1
  
        load(paste(DirCode,'rolling59aicCPI.EX_h',i,'.RData',sep=''))
        
        t=forecast.all[[1]]
        drop=grep('MT.',row.names(t))
        forecast.all=lapply(forecast.all,function(x) x[-drop,])
        # t1=t[[1]]
        save(forecast.all,file=paste(DirCode,'rolling59aicCPI.EX_h',i,'.RData',sep=''))
}
