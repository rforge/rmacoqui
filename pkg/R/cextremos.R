cextremos <-
function(nval) {
            vmax<-0
            vmin<-0 
            if (nval>500)  nval <- 500    
            valores= .Fortran("extremosbaroni",as.integer(nval), 
                                   vmax=as.double(vmax), vmin=as.double(vmin)) 
            return(list(vmin=valores$vmin,vmax=valores$vmax))
            }
