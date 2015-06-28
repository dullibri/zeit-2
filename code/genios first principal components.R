DirCode='h:/Git/zeit-2'
DirCode='C:/Users/Dirk/Documents/GitHub/zeit-2/data/genios'
genios=read.csv(paste(DirCode,'/I_Word_Index_monthly_1991m1-2015m5.csv',sep=''))
fstvar=grep('^IWord$',colnames(genios))
lstvar=grep('^Stagflation$',colnames(genios))
genios.s=genios[13:nrow(genios),5:ncol(genios)]
cor.genios=cor(genios.s)
cor.genios.infl=cor.genios[,1,drop=F]
cor.genios.infl=cor.genios.infl[order(cor.genios.infl,decreasing=T),1,drop=F]
highly.correlated=head(row.names(cor.genios.infl))[2:5]

ind=genios[,fstvar:lstvar]
prc=prcomp(ind,scale=T)
plot(prc$sdev)
fstprc=prc$x[,1]
cor(genios[13:nrow(genios),'Inflation'],fstprc[13:nrow(genios)])

fstprc.highly.correlated=prcomp(genios[,highly.correlated],scale=T)$x[,1]
cor(genios[13:nrow(genios),'Inflation'],fstprc.highly.correlated[13:nrow(genios)])
fstprc=cbind(fstprc,fstprc.highly.correlated)
plot(fstprc)
write.csv(fstprc,paste(DirCode,'/first_principal_component_genios_1991m1-2015m5.csv',sep=''))
