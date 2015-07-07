cardinalesld <-
function (ncorotipos, mldifusa) {
                 ncor <- ncorotipos
                 mres <- array(0, dim=c(ncor, ncor))
                 nf   <- nrow(mldifusa)
                 pos  <- 0
                 ncAux <- 2 + 4*ncor         # +1 : Empiezan las uniones
                 ncDes <- (ncor*(ncor-1)/2)  # Diferencia entre la posicion de una Union y su correspondiente I
                              
                 for (i in 1:(ncor-1)) {
                      for (j  in (i+1): ncor) {
                           pos <- pos + 1
                           mres [i, j] <- mldifusa[nf,ncAux +  pos]            #  UNION 
                           mres [j, i] <- mldifusa[nf,ncAux + ncDes +  pos ]   # INTERSECCion                      
                      }
                 }

                 etiq<-  c(paste("C",1:ncorotipos,sep=""))
             
                 dimnames(mres) <- list(etiq, etiq)
             
#                 dimnames(mres)<-list(c(paste("C",1:ncorotipos,sep=""), paste("G",1:npsg,sep="")),
#                                      c(paste("C",1:ncorotipos,sep=""), paste("G",1:npsg,sep="")))
                 
                 return (mres)
}
