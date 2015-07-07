cmsignosC <-
function(msn, ord=0){
     n        <-  ncol(msn)
     maux     <-  msn
     msc      <-  matrix("",  ncol=n, nrow=n)
     filas    <-  array("",dim=c(n))
     columnas <-  array("",dim=c(n))

#cat("ORd:  ", ord, "\n")
#cat("dim ?Nulo?  :  ", is.null(dimnames(msn)), "\n")
     
#cat("dim:  ", dimnames(msn), "\n")

     if (length(ord)==n & is.vector(ord)) {
         for (i in 1:n) { 
             filas[i]    <- dimnames(msn)[[1]][ord[i]]
             columnas[i] <- dimnames(msn)[[2]][ord[i]]
             for (j in 1:n) {maux[i,j]<-msn[ord[i],ord[j]]}
         }
         dimnames(msc)<-list(filas, columnas)
     }
       
     for (i in 1:(n-1)) {
         for (j in (i+1):n) {
            if (maux[i,j] <  0) msc[i,j] <- switch(abs(maux[i,j]),"-", "--", "---") 
            if (maux[i,j] == 0) msc[i,j] <- "0" 
            if (maux[i,j] >  0) msc[i,j] <- switch(abs(maux[i,j]),"+", "++", "+++") 
         }
     }
     
     return (noquote(msc))
}
