cvaloresmac <-
function(msignos, macoquilistas){
      nvar<-ncol(msignos)
      pos <- 0
      macoquidatos       <- array(0, dim=c(nvar-1, 14))
      macoquiresultados  <- array(0, dim=c(nvar-1, 12))
      macoquiresfin      <- array(0, dim=c(nvar-1, 18))   # Vamos a a?adir las probabilidades de los g's
      
      resvaloresmacoqui = .Fortran("TodosValoresMacoqui", 
                              as.integer(nvar),as.integer(msignos), 
                              as.integer(macoquilistas),
                              as.double(macoquidatos),
                              as.double(macoquiresultados))
        
      macoquidatos       <- matrix(unlist(resvaloresmacoqui[4]), ncol=14)
#      etdat <- c("la", "lb",  "pa","pb","pab","na","nab","nab","a1","b1","ab1","a2","b2","ab2")
      etdat <- c("nA", "nB",  "pA","pB","pAB","mA","mB","mAB","dpA","dpB","d'p","dmA","dmB","d'm")
      dimnames(macoquidatos)<-list(1:nrow(macoquidatos),etdat)

      macoquiresultados  <- matrix(unlist(resvaloresmacoqui[5]), ncol=12)
#      etres <- c("dw", "dwa","dwb","ds","dsa","dsb","gw","gwa","gwb","gs","gsa", "gsb")                              
      etres <- c("DW", "DWA","DWB","DS","DSA","DSB","GW","PGW","GWA","PGWA", "GWB","PGWB","GS","PGS","GSA","PGSA", "GSB","PGSB")                              
      macoquiresfin[,1:7] <- macoquiresultados[,1:7]
      macoquiresfin[,9]   <- macoquiresultados[,8]
      macoquiresfin[,11]  <- macoquiresultados[,9]
      macoquiresfin[,13]  <- macoquiresultados[,10]
      macoquiresfin[,15]  <- macoquiresultados[,11]
      macoquiresfin[,17]  <- macoquiresultados[,12]
      pos <- 6
      for (i in 1:6) {
          pos <- pos + 1 
          for (j in (1:(nvar-1))) {
              macoquiresfin[j,pos+i] <- ifelse(macoquiresultados[j, pos] == -99, -99,1-pchisq(macoquiresultados[j,pos],1))
          }
      }

      dimnames(macoquiresfin)<-list(1:nrow(macoquiresfin),etres)
      
## Valores -99 (missing) los convertiremos en NA
##      macoquiresfin [macoquiresfin==-99] <- NA
      
      return(list(macdatos=macoquidatos, macres=macoquiresfin))
      }
