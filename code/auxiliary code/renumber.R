renumber=function(df){
        # df is a sorted frame where some elements have been eliminated. 
        # This functions replaces the ranks 
        # accordingly taking care of left out rows.
        ts=round(sapply(1:12,function(x) rank(df[,x])),0)
        return(ts)
}