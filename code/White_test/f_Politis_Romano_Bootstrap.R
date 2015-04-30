#function BtstrpMat = f_Politis_Romano_Bootstrap( alternative , n , blockparam, display,flag,mat)
f_Politis_Romano_Bootstrap = function(alternative, n, blockparam, display, flag, mat)
{
        #  It is exactly coded as in the paper:
        #    "The Stationary bootstrap" by Dimitris N. Politis and Joseph P. Romano
        #  The exact description can be found on page 4 in this paper
        #  link existed at the date of creation  ==> 
        #    http://www.stat.purdue.edu/research/technical_reports/pdfs/1991/tr91-03.pdf
        # 
        #  Note you could easily replace the if loop with geornd too speed things
        #  up, but it is done this way for educational sake and to folow the exact
        #  description of the paper!
        # 
        # by Arnout Tilgenkamp
        # 21 Dec 2011 (Updated 15 Oct 2013) 
        # http://www.mathworks.com/matlabcentral/fileexchange/34306-white-reality-check/content//PolitisRomanoBootstrap.m
        #
        #
        # Converted to R by Konstantin A. Kholodilin
        # DIW Berlin
        # kkholodilin (at) diw . de
        
        # First created --- April 15, 2015  									
        # Last modified --- April 15, 2015
        #-------------------------------------------------------------------------------
        
        
        NRow = nrow(alternative)
        NCol = ncol(alternative)
        BtstrpMat = rep(NA, n*NCol)
        dim(BtstrpMat) = c(n, NCol)
        
        for(i in 1:n)
        {
                if(display == 1){print(paste('simulations done: ', i, sep=""))}
                
                New = rep(NA, NRow*NCol)
                dim(New) = c(NRow, NCol)
                
                tel = 0
                while(tel < NRow)
                {
                        tel = tel + 1
                        p = runif(1)
                        if(p < blockparam || tel == 1)
                        {
                                row = sample.int(NRow, size = 1, replace=T)
                                Xnext = alternative[row,]
                        }else{
                                row = row + 1;
                                if(row > NRow){row = row - NRow}
                                
                                Xnext = alternative[row,]
                        }
                        New[tel,] = Xnext
                } # end of while loop
                
                if(flag == 1) {f = -New^2 - mat^2}
                if(flag == 2) {f = log(1+New)-log(1+mat)}
                if(flag == 3) {f = -abs(New) + abs(mat)}
                
                froof = colMeans(f)
                BtstrpMat[i,] = froof
                
        } # end of loop
        
        return(BtstrpMat)
}