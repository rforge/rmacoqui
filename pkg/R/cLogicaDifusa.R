cLogicaDifusa <-
function (ncorotipos, elc) {
             ncor <- ncorotipos   
             nvar   <- nrow(elc) - 1
             ncolum <- 2 + 4*ncor + ncor*(ncor-1)
#             ncolum <- 2 + ncor*(ncor-1)
             mres   <- array(0, dim=c(nvar+1, ncolum))

             for (i in 1:nvar) {
                  mres[i,1] <- elc[i,1]
                  mres[i,2] <- elc[i,2]                  
                  for (j in 1:ncor) {
                      mres[i,2+4*(j-1)+1] <- elc[i,2+j]                        # Grado de pertenencia al grupo j
                      mres[i,2+4*(j-1)+2] <- 1 - elc[i,2+j]                    # Grado de pertenencia al complementario del  grupo j
                      mres[i,2+4*(j-1)+3] <- max(elc[i,2+j],1 - elc[i,2+j])    
                      mres[i,2+4*(j-1)+4] <- min(elc[i,2+j],1 - elc[i,2+j])    
                  }
             }
#             
#BLOQUE 3: Uniones e Intersecciones. Si el numero de corotipos es mayor de 1:
#
if (ncor > 1) {
           
# Grado de pertenencia a UNION:u(di,dj)--> max(di,dj) e Interseccion: i(di,dj)--> min(di,dj)
             for (i in 1:nvar) {
                  n <- 2 + 4*ncor
#                  n <- 2 
                  for (j in 1:(ncor-1)) 
                      for (k in (j+1):(ncor)) {
                           n <- n + 1
                           mres[i,n] <- max(elc[i,2+j], elc[i,2+k])                      # UNION
                           mres[i,(ncor*(ncor-1)/2)+n] <- min(elc[i,2+j], elc[i,2+k])    # INTERSECCION
                  }
             }
}

#
# Cardinales (Totales)
#
             sumascol <- colSums(mres)
             for (i in 3:ncolum)  mres[nvar+1,i] <- sumascol[i] 
#
#  Definicion de Nombres Columnas/filas: dimnames

#
#  Nombres Columnas/filas: dimnames
#
             ds  <- paste("d",   1:ncor, sep="")
             cds <- paste("cd",  1:ncor, sep="")

             ud  <- c(paste("U(C",1:ncorotipos,",cC",1:ncor,")",  sep=""))
             id  <- c(paste("I(C",1:ncorotipos,",cC",1:ncor,")",  sep=""))
             
             uds1 <- c(paste("U(C", 1:ncorotipos,",", sep="") )
             uds2 <- c(paste("C",   1:ncorotipos,")", sep="") )
             
             ids1 <- c(paste("I(C", 1:ncorotipos,",", sep="") ) 
             ids2 <- c(paste("C",   1:ncorotipos,")", sep="") )
            
             
             cabec <- cbind(ds[1], cds[1], ud[1], id[1])

             if (ncor > 1) {
               for (i in 2:ncor) cabec<-cbind(cabec, ds[i], cds[i],ud[i], id[i])   
                            
               for (i in 1:(ncor-1)) for (j in (i+1):ncor){ cabec <- cbind(cabec, paste(uds1[i],uds2[j]))}             
  
               for (i in 1:(ncor-1)) for (j in (i+1):ncor){ cabec <- cbind(cabec, paste(ids1[i],ids2[j]))}             
             }



             if (lang=="es" ) {
                dimnames(mres)<-list(c(seq(1:nvar),"Cardinal"),
                                  c("Especie", "Corotipo",cabec))
             }
             else {
                 dimnames(mres)<-list(c(seq(1:nvar),"Cardinal"),
                                  c("Species", "Chorotype",cabec))           
             }

             
             return(mres)
}
