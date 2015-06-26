t=sapply(fbs,function(x) x[1,])
rownames.fbs=row.names(result)[1:(nrow(result)-2)]
for (i in 1:12){
        row.names(fbs[[i]])=rownames.fbs
}
nmod=length(sel.mods)
t=sapply(fbs,function(x) colSums(x[sel.mods,],na.rm=T))
tl=sapply(t,length)
arfb=data.frame(matrix(NA,ncol=tl[1],nrow=length(t)))
colnames(arfb)=names(t[[1]])
for (i in 1:length(t)){
        inn=colnames(arfb)[colnames(arfb)%in%names(t[[i]])]
     arfb[i,inn]=t[[i]][1:length(inn)]#*(i-2)
}
# arfb=arfb[-c(1,2,15),]
plot(colSums(arfb,na.rm=T)/nmod*max.hor*100,type='l')
instablephase=colSums(arfb,na.rm=T)>nmod*max.hor*0.20
plot(instablephase,type='l')

# 
# abline(v=grep('2010-06',colnames(arfb)))
# abline(v=grep('2008-12',colnames(arfb)))
# arfb.end=arfb[9:12,grep('2007-01',colnames(arfb)):ncol(arfb)]
# par.s=par()
# pdf(paste(DirCode,'/figs/ar_fbp_9_12.pdf',sep=''))
# 
# par(las=2
#     ,mar=c(5.1,4,2,2.1)
#            )
# plot(unlist(arfb.end[4,])
#      ,ylim=c(8.5,12.5)
#      , xaxt = 'n'
#      ,yaxt='n'
#      ,ylab='forecast horizon'
#      ,xlab=''
#      )
# lines(unlist(arfb.end[3,]),type='p')
# lines(unlist(arfb.end[2,]),type='p')
# lines(unlist(arfb.end[1,]),type='p')
# ticks=seq(1,ncol(arfb.end),5)
# labels=colnames(arfb.end)
# axis(1, at=ticks, labels=labels[ticks]
# #      ,las=2
#      )
# axis(2,at=c(9,10,11,12),labels=c('9','10','11','12'))
# abline(v=grep('2008-05',colnames(arfb.end)))
# abline(v=grep('2009-01',colnames(arfb.end)))
# par(par.s)
# 
# dev.off()
# write.csv(t(arfb.end),paste(DirCode,'/results/arfb.end.csv',sep=''))
# arfb[is.na(arfb)==T]=0
# 
# write.csv(t(arfb),paste(DirCode,'/results/arfb.csv',sep=''))
# # looking at industrial production
# ips=data[grep(colnames(arfb.end)[1],data$ym):grep(colnames(arfb.end)[ncol(arfb.end)],data$ym),'IP-untr']
# 
# pdf(paste(DirCode,'/figs/ip_crisis.pdf',sep=''))
# par(las=2
#     ,mar=c(5.1,4,2,2.1)
# )
# plot(ips
# #      ,ylim=c(8.5,12.5)
#      , xaxt = 'n'
# #      ,yaxt='n'
#      ,ylab=''
#      ,xlab=''
# ,type='l'
# )
# ticks=seq(1,ncol(arfb.end),5)
# labels=colnames(arfb.end)
# axis(1, at=ticks, labels=labels[ticks]
#           ,las=2
# )
# axis(2,at=c(9,10,11,12),labels=c('9','10','11','12'))
# abline(v=grep('2008-05',colnames(arfb.end)))
# abline(v=grep('2009-01',colnames(arfb.end)))
# dev.off()
# par(par.s)
# 
# 
# pdf(paste(DirCode,'/figs/ar_fbp_9_12.pdf',sep=''))
# 
# par(las=2
#     ,mar=c(5.1,4,2,2.1)
# )
# 
# plot(colSums(arfb)>171*12/4
# #      ,ylim=c(8.5,12.5)
#      , xaxt = 'n'
#      ,yaxt='n'
#      ,ylab='forecast horizon'
#      ,xlab=''
# ,type='l'
# )
# ticks=seq(1,ncol(arfb),5)
# 
# labels=colnames(arfb)
# axis(1, at=ticks, labels=labels[ticks]
#      #      ,las=2
# )
# axis(2,at=c(9,10,11,12),labels=c('9','10','11','12'))
# abline(v=grep('2008-05',colnames(arfb)))
# abline(v=grep('2009-01',colnames(arfb)))
# abline(v=grep('2010-01',colnames(arfb)))
# par(par.s)
# 
# dev.off()