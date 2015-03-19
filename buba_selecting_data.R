# DirCode='C:/Users/Dirk/Documents/GitHub/bubblesbreakdowns'
DirCode='h:/Git/zeit-2/data/buba/data'
df=read.csv(paste(DirCode,'/Buba_Detailed_Variables_List_EN.csv'
                            ,sep='')
                      ,sep=','
                      ,stringsAsFactors=F
#                       ,row.names=1
)
t=df[,2]
tt=grep('[mM]oney market rate',t)
money.rate=df[tt,]
tt=grep('[Mm]onthly average',money.rate[,2])
money.rate=money.rate[tt,]
