fuzzy.Clusters <-
function (macoquires, grupos) {
#
# Calculos grupo a grupo... Evitamos considerar hijos-padres, aunque habra que 
# tener cuidado con elgruposRes (elementos de cada grupo).
#
#  Comprobacion de grupos correctamente definidos: gruposcorrectos(grupos, nvar)  <- 
#

    ngrupos          <- length(grupos)/2
    elgrupos         <- array (0, dim=c(macoquires$nvar, 1+ngrupos))  # Pertenencia de la especie al grupo
    elgrupos_aux     <- array(0, dim=c(macoquires$nvar))
    dis_elgrupos     <- array (-1, dim=c(macoquires$nvar+1, 2+ngrupos))
    dis_elgrupos_aux <- array (-1, dim=c(macoquires$nvar+1, 2+ngrupos))
    grcor            <- array(-1, dim=c(2,macoquires$nvar-1))
    desGrupos        <- array(-9, dim=c(ngrupos,4))    
    ncf              <- 4     # N?mero de columnas fijas en la matriz Entropia: Cardinal - Entropia - Altura - Especies_N

# primer grupo:
   # Rama izquierda
   if(grupos[2] == 1) {grcor[1,grupos[1]] = 0  }
   # Rama derecha
   if(grupos[2] == 2) {grcor[2,grupos[1]] = 0  }
   
   for (i in 1:macoquires$nvar) elgrupos[i,1] <- i
   elgrupos_aux <- elecorotipos (macoquires$maclistas, grcor)
   elgrupos[,2] <- elgrupos_aux
   
   dis_elgrupos_aux <- cmDCor(1, elgrupos_aux, macoquires$mprox)
   dis_elgrupos[,1:3] <- dis_elgrupos_aux
#   dis_elgrupos <- dis_elgrupos[,-2]               # No queremos la segunda columna
    
# Resto de grupos
   if (ngrupos > 1) {
      for (i  in 2:ngrupos)  { 
         grcor <- array(-1, dim=c(2,macoquires$nvar-1))
         if(grupos[2*i] == 1) {
            grcor[1,grupos[2*i-1]] = 0
         }
         if(grupos[2*i] == 2) {
            grcor[2,grupos[2*i-1]] = 0  
         }
         elgrupos_aux   <- elecorotipos (macoquires$maclistas, grcor)
         elgrupos[,1+i] <- elgrupos_aux
         
         dis_elgrupos_aux <- cmDCor(1, elgrupos_aux, macoquires$mprox)
         dis_elgrupos[,2+i] <- dis_elgrupos_aux[,3]
      } 
    }

          ldifusaRes      <- cLogicaDifusa(ngrupos, dis_elgrupos)
          
          
          mcardinalesRes <-0
          mentropiaRes <-0
          if (ngrupos > 1) {
              mcardinalesRes  <- cardinalesld(ngrupos, ldifusaRes)
              mentropiaRes    <- centropia(ngrupos,ldifusaRes)
              
# Calculamos el n?mero de especies que hay en cada grupo
              for (i in 1:ngrupos)  mentropiaRes[i,ncf] <- sum (elgrupos[,1+i])            
          }


# ETIQUETAS
#
# Dependientes del lenguaje
#
          if (lang=="es") {
              etdesGrupos <- c("Id", "Cluster", "Nodo","Rama")
              etelGrupos  <- "Especies" 
              etdisGrupos <- c("Especie", "Corotipo", paste("Pertenencia G",1:ngrupos,sep="") )
              etmentropia <- c("Cardinal", "Entropia","Altura", "Especies_N", paste("G",1:ngrupos," In", sep=""))
# Mensajes para imprimir los resultados
              msg1 <- paste(" \n\nGRUPOS \n========= \n  Numero de Grupos:                         ", ngrupos,"\n\n")
              msg2a <- paste( " \n\nLogica Difusa:\n==============\n\n")
              msg2 <- paste( " \n\nIntersecciones y uniones entre grupos\n\n")
              msg3 <- paste("\n\nCardinal Interseccion\\Union:\n=============================  \n\n")
              msg4 <- paste(" \n\nParametros de Logica Difusa:\n=========\n\n")
              msg5 <- paste(" \n\nInclusion:\n==========\n\n")
              msg6 <- paste("\n\nSuperposicion Borrosa:\n======================\n\n")

              }
          else {
              etdesGrupos <- c("Id", "Cluster", "Node","Branch")
              etelGrupos  <- "Species" 
              etdisGrupos <- c("Species", "Chorotype", paste("Membership G",1:ngrupos,sep="") )
              etmentropia <- c("Cardinal", "Entropy","Height","Species_N", paste("G",1:ngrupos," In", sep=""))
# Mensajes para imprimir los resultados
              msg1 <- paste(" \n\nCLUSTERS \n========== \n  Number of Clusters:                    ", ngrupos,"\n\n")
              msg2a <- paste( " \n\nFuzzy Logic\n===========\n\n")
              msg2 <- paste( " \n\nIntersection and union between clusters\n\n")
              msg3 <- paste("\n\nCardinality Intersection\\Union\n===============================  \n\n")
              msg4 <- paste(" \n\nCluster fuzzy parameters\n========================\n\n")
              msg5 <- paste(" \n\nInclusion:\n==========\n\n")
              msg6 <- paste("\n\n\nFuzzy Overlap  \n=============\n\n")
          }
#  Descripcion de Grupos:  desGrupos

          for (i in 1:ngrupos) {
            desGrupos[i,1] <- i
            desGrupos[i,2] <- paste("G", i, sep="")
            desGrupos[i,3] <- grupos[2*i-1]
            if ( grupos[2*i] == 1 ) desGrupos[i,4] <- "A"
            if ( grupos[2*i] == 2 ) desGrupos[i,4] <- "B"
          }
          dimnames(desGrupos) <- list(1:(ngrupos), etdesGrupos)

#  

#  Elementos de cada Grupo:  elGrupos

         etelGrupos <- c(etelGrupos, c(paste("G",1:ngrupos,sep="")))
         dimnames(elgrupos) <- list(1:(macoquires$nvar), etelGrupos)

#  

#  Pertenencia de cada especia a cada grupo: dis_elgrupos

         dimnames(dis_elgrupos)<-list(c(seq(1:macoquires$nvar),"Cardinal"), etdisGrupos)

#  Logica Difusa: ldifusaRes
             ds  <- paste("d",   1:ngrupos, sep="")
             cds <- paste("cd",  1:ngrupos, sep="")

             ud  <- c(paste("U(G",1:ngrupos,",cG",1:ngrupos,")",  sep=""))
             id  <- c(paste("I(G",1:ngrupos,",cG",1:ngrupos,")",  sep=""))
             
             uds1 <- c(paste("U(G", 1:ngrupos,",", sep="") )
             uds2 <- c(paste("G",   1:ngrupos,")", sep="") )
             
             ids1 <- c(paste("I(G", 1:ngrupos,",", sep="") ) 
             ids2 <- c(paste("G",   1:ngrupos,")", sep="") )
            
             
             cabec <- cbind(ds[1], cds[1], ud[1], id[1])

             if (ngrupos > 1) {
               for (i in 2:ngrupos) cabec<-cbind(cabec, ds[i], cds[i],ud[i], id[i])   
                            
               for (i in 1:(ngrupos-1)) for (j in (i+1):ngrupos){ cabec <- cbind(cabec, paste(uds1[i],uds2[j]))}             
  
               for (i in 1:(ngrupos-1)) for (j in (i+1):ngrupos){ cabec <- cbind(cabec, paste(ids1[i],ids2[j]))}             
#  MCARDINALES: mcardinalesRes

               dimnames(mcardinalesRes) <- list(paste("G",1:ngrupos,sep=""),
                                              paste("G",1:ngrupos,sep=""))
              
#  MENTROPIA: mentropiaRes

               dimnames(mentropiaRes) <- list(c(paste("G",1:ngrupos,sep="")), 
                                            c(etmentropia,paste("G",1:ngrupos, sep="")))
             }

             if (lang=="es" ) {
                dimnames(ldifusaRes)<-list(c(seq(1:macoquires$nvar),"Cardinal"),
                                  c("Elemento", "Grupo",cabec))
             }
             else {
                 dimnames(ldifusaRes)<-list(c(seq(1:macoquires$nvar),"Cardinal"),
                                  c("Element", "Group",cabec))           
             }

# PINTAMOS RESULTADOS

    cat(msg1)
    print(desGrupos)
    cat("\n\n")
    print(elgrupos)
    cat("\n\n")
    cat(msg2a)
    print(round(dis_elgrupos[,-2],digits=3))

    cat(msg2)
    if (ngrupos ==1) {print (round(cbind(ldifusaRes[,1],ldifusaRes[,5:(ncol(ldifusaRes))]), digits=3))}

    if (ngrupos>1) {
        print (round(cbind(ldifusaRes[,1],ldifusaRes[,(((ngrupos)*4 +3):(ncol(ldifusaRes)))]), digits=3))
        cat(msg3)
        print (round(mcardinalesRes, digits=3))

#Cluster fuzzy parameters    
        cat(msg4)
        print (round(mentropiaRes[,1:ncf], digits=3))
    
        cat(msg5)
        print (round(mentropiaRes[,(ncf+1):(ncf+ngrupos)], digits=3))
    
        cat(msg6)
        print (round(mentropiaRes[,(ncf+ngrupos):ncol(mentropiaRes)], digits=3))
    }
          fuzzyRes <- list(ngrupos=ngrupos, grupos = grupos, 
                           desGrupos = desGrupos,
                           elementosGrupos1 = elgrupos,
                           elementosGrupos2 = dis_elgrupos[,-2],
                           ldifusaGrupos=ldifusaRes[,-2], 
                           mcardinales=mcardinalesRes, 
                           mentropia=mentropiaRes,
                           datos = macoquires$datos,
                           isprox = macoquires$isprox 
                           )

class(fuzzyRes) <- "RFuzzy"
invisible(fuzzyRes)
}
