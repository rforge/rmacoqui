locCorotGrupos <-
function (fuzzyres, grupos) {

     ngrupos <- length(grupos)/2
     elcor <- fuzzyres$elementosGrupos2
     datos <- fuzzyres$datos

#     ncor   <- ncol(elcor) - 2
     ncor   <- ngrupos
     
     nvar   <- nrow(elcor) - 1
     nf     <- nrow(datos)
     nc     <- ncol(datos)
     mres   <- array(0, dim=c(nf, 1 + 4*ncor))
     aux    <- array (0, dim=c(nvar))
     vcor   <- array (0, dim=c(nvar))
     pcor   <- array (0, dim=c(nvar))    # Pertenencia de la especie al corotipo

     t_elcor <- t(elcor)


if (fuzzyres$isprox == 1) stop ("This function is only avalaible if Macoqui was run with presence/absence data ")

## Esta funcion puede tardar en obtener los resultados.
## Algun tipo de reloj
     
     print ("This process can take some computing time.")
      
     for (j in 1:ngrupos) {

# Valores  de 'membresia al corotipo/grupo"
         pcor  <- t_elcor[1+j, 1:nvar]

# Para marcar que especies est?n en el corotipo/grupo j
#         vcor   <- t_elcor[2, 1:nvar]
#         vcor [vcor!=j]    <- 0
#         vcor [vcor == j ] <- 1       # Queda con 1 cuando pertenece a ci y 0, si no.
         vcor <- fuzzyres$elementosGrupos1[,1+j]
# C?lculos:

         for (i in 1:nf) {
             mres[i,1] <- i
             aux <- sum(datos[i, ] * vcor  )
             mres[i,1+(j-1)*4+2] <- aux                     # SR
             if (aux>0) mres[i,1+(j-1)*4+1] <- 1            # P

             aux <- datos[i, ]* vcor * pcor
             mres[i,1+(j-1)*4+3] <- max(datos[i, ] * pcor)   # MMD
             mres[i,1+(j-1)*4+4] <- sum(datos[i, ] * pcor)   # FSR
         }
     }

#
# Etiquetas
#
      p<-paste("P",1:ncor, sep="")
      sr<-paste("SR",1:ncor, sep="")
      mmd<-paste("MMD",1:ncor, sep="")
      fsr<-paste("FSR",1:ncor, sep="")
      et <- c("Loc", p, sr, mmd, fsr)

      pos <- 1
      et2<-et

      for (i in 1:ncor) {
        for (j in 1:4) {
            pos <- pos + 1
            et2[pos] <- et [1+i+(j-1)*ncor]
        }
      }
      dimnames(mres) <- list(1:nf, et2)
      print (mres)

      fuzzyres[["locGrupos"]] <- mres       # Asignamos globalmente (<<) locCorotipos
      fuz2 <- fuzzyres
   #   return (mres)

      invisible (fuz2)
}
