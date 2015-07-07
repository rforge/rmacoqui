centropia <-
function(ncorotipos, mlogbor) {
             ncor <- ncorotipos 
             ncf  <- 4    # N?mero de columnas fijas en  la matriz resultado (mres):    Cardinal - Entropia - Altura - NEspecies (4)
             mres       <- array(0, dim=c(ncor, ncf + ncor + ncor))  # ncf + ncor + ncor :Cardinal - Entropia - Altura - NEspecies - MInclusion - GradoSolapamiento
             minclusion <- array(0, dim=c(ncor, ncor))    
             nf         <- nrow(mlogbor)
             nI         <- 2 + 4*ncor + (ncor*(ncor-1)/2)  # +1 : Empiezan las intersecciones
             ncAux      <- 2 + 4*ncor         # +1 : Empiezan las uniones
             ncDes      <- (ncor*(ncor-1)/2)  # Diferencia entre la posicion de una Union y su correspondiente I
             pos <- 0
             
             for (i in 1:ncor) {
                  mres[i,1] <-  mlogbor[nf, 2 + (i-1)*4 + 1]                            # Car (Corotipo i) 
                  mres[i,2] <- (mlogbor[nf,2+4*(i-1)+4])/(mlogbor[nf,2+4*(i-1)+3])      # Entropia
                  mres[i,3] <-  max(mlogbor[1:nf-1, 2 + (i-1)*4 + 1])                   # Altura: M?ximo Grado pertenencia                  
             } 

# N?mero de especies en cada corotipo
             for (i in 1:ncor) mres[i,4]<-0 
             for (i in 1:(nf-1)) mres[mlogbor[i,2],4]<- mres[mlogbor[i,2],4] + 1 

# inclusion de j en i 
             for (i in 1:(ncor-1)) {
                  for (j in (i+1):ncor) {
                      pos <- pos + 1
                      mres[i, ncf + j] <-  mlogbor[nf,nI + pos]/ mres[j,1]  
                      mres[j, ncf + i] <-  mlogbor[nf,nI + pos]/ mres[i,1]
                  }
                  mres[i, ncf + i] <- 1        # Elementos de la diagonal                  
             }
             mres[ncor,ncf + ncor] <- 1        # Elementos de la diagonal 
             
# Grado solapamiento de i en j (sim?trica) =   car(I(cor1, cor2)) / car(U(cor1,cor2))
             pos <- 0
             for (i in 1:(ncor-1)) {
                  for (j  in (i+1): ncor) {
                       pos <- pos + 1
                       mres [i, ncf + ncor + j] <- (mlogbor[nf,ncAux + ncDes +  pos ]) / (mlogbor[nf,ncAux +  pos])
                       mres [j, ncf + ncor + i] <- mres [i, ncf + ncor + j]
                       
                  }
                  mres[i, ncf + ncor + i] <- 1        # Elementos de la diagonal                  
              }
             mres[ncor,ncf + ncor + ncor] <- 1        # Elementos de la diagonal 

# Etiquetas Dimensiones   
                 etfil<-  c(paste("C",1:ncorotipos,sep=""))
#                 if (npsg>0) etfil <- c(etfil,  paste("G",1:npsg,sep=""))
                 
                 if (lang=="es")   {
                      etcol <- c("Cardinal", "Entropia","Altura","Especies_N",  paste("C",1:ncorotipos," In", sep=""))
                 }
                 else {
                      etcol <- c("Cardinal", "Entropy","Height","Species_N", paste("C",1:ncorotipos," In", sep=""))
                 }
                 
#                 if (npsg>0) etcol <- c(etcol, paste("G",1:npsg," In", sep=""))
                 etcol <- c(etcol,paste("C",1:ncorotipos, sep=""))
#                 if (npsg>0) etcol <- c(etcol, paste("G",1:npsg, sep=""))

                 dimnames(mres) <- list(etfil, etcol)

             
             return(mres)
}
