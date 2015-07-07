ordenaFilas <-
function(matini, vorden){
     nf<-nrow(matini)
     nc<-ncol(matini)
     eticol <- array (0, dim =c(nc))
     mres <- array(0, dim=c(nf,nc))
     for (i in 1:nf) {
            mres[i,]  <- matini[vorden[i],]
     }
# Etiquetas filas y columnas 
     for (i in 1:nc) eticol[i] <- colnames(matini)[[i]]
     dimnames (mres) <- list( c(1:(nf-1),"Cardinal"), eticol)
     return(mres)
}
