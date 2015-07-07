renumber.s=function(df){
        # df is a sorted frame where some elements have been eliminated. 
        # This functions replaces the ranks 
        # accordingly taking care of left out rows.
        ts=rank(df[,1],0)
        return(ts)
}