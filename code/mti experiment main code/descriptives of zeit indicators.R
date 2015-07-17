total.used=total[rows.used,]
rows.used=total.used$m<=4&total.used$year.x==2014
total.used=total.used[!rows.used,]
descriptives=data.frame(narticles=nrow(total.used)
                        ,ininflationarticles=sum(total.used$Inflation))
