reordenaMatriz <-
function(matini, vorden){
     n<-ncol(matini)
     etiquetas <- array (0, dim =c(n))
     mres <- array(0, dim=c(n,n))
     for (i in 1:n) {
        for (j in 1: n) {
            mres[i,j]  <- matini[vorden[i],vorden[j]]
        }
     }
# Etiquetas filas y columnas 
     for (i in 1:n) etiquetas[i] <- rownames(matini)[[vorden[i]]]
     dimnames (mres) <- list( etiquetas, vorden)
     return(mres)
}
