macoqui <-
function (datos, lang="en", isprox=0, nloc = 0, vmax=0, vmin=0){
          metodo      <- "baroni"                            # Solo baroni. Habraque quitar esta linea para dejar mas metodos.
          nvar        <- ncol(datos)
          nval        <- nrow(datos)
          mprox       <- matrix(0, ncol=nvar, nrow=nvar)     # Matriz de prox
          mproxOrdC   <- matrix(0, ncol=nvar, nrow=nvar)     # Matriz de prox ordenada segun cluster
          msignosN    <- matrix(0, ncol=nvar, nrow=nvar)     # Matriz de signos numericas: -1, 0, 1
          msignosC    <- matrix("", ncol=nvar, nrow=nvar)    # Matriz de signos caracter:   -, 0, +
# Var. de Corotipos
          ncor        <- 0                                   # Numero de corotipos
          corotiposSN <- array(-9, dim=c(nvar-1,2))          # Indica si un nodo es corotipo o donde incumple la condicion
          corotiposSN_aux <- array(-9, dim=c(nvar-1,2))
          ncor12        <- 0                                 # Numero de corotipos
          corotiposSN12 <- array(-9, dim=c(nvar-1,2))        # Indica si un nodo es corotipo o donde incumple la condicion
          desCorotipos <- 0
          elcor <- 0
          dis_elcor   <- 0
#          ncor_psg    <- 0                                   # Numero de patrones de sustitucion gradual

# Variables para Logico Difusa: Matriz de Baroni
          ldifusa     <- 0
          mentropia   <- 0
          mcardinales <- 0


# nloc: Numero de localidades (filas en la matriz de datos de ausencias/presencias)
#      Si los datos son una matriz de proximidades --> tendriamos que definir nloc, no vale que sea 0
#


#
# Asignacion Global del lenguaje que usaremos
# Por defecto sera ingles: en, aunque podemos admitir espanol:es.

         assign("lang", lang, .GlobalEnv)


# Comprobamos si tenemos nloc

        if (isprox == 1) {
#            if(nloc == 0 ) {stop ("Proximities matrix, you can define localization number") }
            if(nloc == 0 ) {stop ("The input is a proximity matrix. The number of localities considered must be specified.") }
            else { nval <- nloc}
        }

#          
#C?lculo de vmin/vmax
          
          if (vmax==0 & vmin==0) {               # Se toman los valores por defecto. Los calculamos.
              extremos    <- cextremos(nval)
              if (nval >= 500) warning ("Number of attributes >500. Critical values not available for N>500. Values for N=500 used by default.\n If other values are preferred, call the macoqui function stating alternative vmax and vmin")
          } else {
              if (vmax < vmin) stop ("vmax/vmin incorrects, check your values")
              extremos <- list(vmin = vmin, vmax=vmax)         
          }

# Tipo de matriz de datos segun metodo          

          tmatriz <- ifelse(metodo=="baroni", 1,
                     ifelse(metodo=="jaccard",2,
                     ifelse(metodo=="pearson" | metodo=="kendall" | metodo=="spearman", 3 , 0)))

#
#   C?lculo de la matriz de proximidades
#
# Puede que los datos de entrada sean una matriz de prox (isprox=1)
#           
          if (isprox == 1) {
              nval<- nloc

              rr <- mProx_valida(datos)  # validamos que sea una matriz de similitudes
              if (rr$mprox_ok) {
                  mprox <- rr$mprox
                } else stop("Not proximities matrix, check your values")
          }  else mprox <-  cmProx(datos, metodo)

          
#          msignosN    <- cmsignos(mprox,datos, nval, metodo)
          msignosN    <- cmsignos(mprox, datos, extremos, metodo)

          clres       <- maccluster(mprox, tmatriz)
          mproxOrdC   <- reordenaMatriz(mprox, clres$iord)

          msignosC    <- cmsignosC(msignosN, clres$iord)
          maclistas   <- cmaclistas (clres$ilc, clres$iuc)

          macvalores  <- cvaloresmac(msignosN, maclistas$maclistas)

          ccoros      <- ccorotipos (maclistas$etprevial, maclistas$etpreviau,
                         macvalores$macres[,2],    # dwa
                         macvalores$macres[,3],    # dwb
                         macvalores$macres[,9],    # gwa
                         macvalores$macres[,11])   # gwb

          ncor        <- ccoros$ncor
          corotiposSN <- ccoros$corotiposSN
#          ncor12 <- ccoros$ncor
#          corotiposSN12 <- ccoros$corotiposSN

########################################################################################################
#
#
#                                         SI EL NCOR > 0
#
#
# Tipos de Corotipos:
#    Tipo 1: Cuando el dw correspondientes vale 1
#         2: Cuando dw no es 1
#
##    SOLO TRABAJAREMOS CON LOS COROTIPOS TIPO 1 Y TIPO 2.
##
##   SE CREAR? LA FUNCion FuzzyClusters() para tratar grupos independientemente sean o no corotipos.
#
##  Se borra todo el c?lculo de corotipo tipo 3 y el de patrones graduales.
##  Vease en la version funcionesRMacoqui de noviembre.
#

if (ncor > 0 ) {

# Descripcion de los corotipos:
        desCorotipos <- array(-9, dim=c(ncor,5))    # Vector con la descripcion de los corotipos: Id, Agrupacion, Clase, Nodo y Rama

# Buscamos Corotipos
        pos <- 0
        # Por la izquierda: 1 - l
            for (i in 1:(nvar-1)) {
                if (ccoros$corotiposSN[1,i] == 0 ) {
                    pos <- pos + 1
                    desCorotipos[pos,1] <- pos
                    desCorotipos[pos,2] <- paste("C",pos, sep="")
                    desCorotipos[pos,3] <- "Chorotype"
                    desCorotipos[pos,4] <- i
                    desCorotipos[pos,5] <- "A"
                }
        # Por la derecha: 2 - u
                if (ccoros$corotiposSN[2,i] == 0 ) {
                    pos <- pos + 1
                    desCorotipos[pos,1] <- pos
                    desCorotipos[pos,2] <- paste("C",pos, sep="")
                    desCorotipos[pos,3] <- "Chorotype"
                    desCorotipos[pos,4] <- i
                    desCorotipos[pos,5] <- "B"
                 }
            }

#          etdesCor <- c("Tipo", "Nodo/Elemento","Id","fila","col")

          if (lang=="es") {
              etdesCor <- c("Id", "Agrupacion", "Clase","Nodo","Rama")
              }
          else {
              etdesCor <- c("Id", "Grouping", "Class","Node","Branch")
          }
          dimnames(desCorotipos) <- list(1:(ncor), etdesCor)

# elcor: Elementos - Corotipos: Vector indicando el corotipo al que pertenece cada especie
#        elcor(i) indica el corotipo al que pertenece la especie i

          elcor       <- elecorotipos (maclistas$maclistas, corotiposSN)

# Grados de pertenencia de cada especie a cada corotipo
# Asi, dis_elcor(i,j) , con 1=1-nvar y j=3, ncor+2 indica:
#      grado de pertenencia de la especie i al corotipo j
#    dis_eclcor(i,1): Indica el elemento
#    dis_eclcor(i,2): Indica el corotipo al que pertenece el elemento

          dis_elcor   <- cmDCor(ncor, elcor, mprox)

#
# LOGICA DIFUSA:  Solo en caso de metodo = BARONI
#
        
#    
# Si ncor=1 no hay intersecciones/uniones: ?C?mo se calcular Entropia/Cardinales
#
          if ((ncor) > 0 & metodo == "baroni") {
              ldifusa     <- cLogicaDifusa(ncor, dis_elcor)
              if (ncor>1) {
                  mcardinales <- cardinalesld(ncor, ldifusa)
                  mentropia   <- centropia(ncor,ldifusa)
              }
          }


##############################
#
#     Resultados de corotipos en Localidades
#
#  Printamos los datos pero calculamos la matriz de los Corotipos en las localidades
#

#####    mCorLoc      <- array(0, dim=c(nval, nvar + 1 + 4*ccoros$ncor))
#####    locCorot_aux <- array(0, dim=c(nval, 1 + 4*ccoros$ncor))

}

#                                   FIN DE NCOR > 0
#
########################################################################################################

          macvalores$macres[macvalores$macres==-99] <- NA
          
    macoquires<-list(nvar=nvar, nval=nval, extremos=extremos,
                metodo=metodo, tmatriz=tmatriz,mprox=mprox, mproxOrdc = mproxOrdC,
                msignosN=msignosN, msignosC=msignosC,
                ilc=clres$ilc, iuc=clres$iuc, x=clres$x, cd=clres$cd,
                dord=clres$dord, iord=clres$iord,dend=clres$dend,
                etprevial=maclistas$etprevial, etpreviau=maclistas$etpreviau,
                maclistas=maclistas$maclistas,
                macdatos=macvalores$macdatos, 
                macres=macvalores$macres,
                corotiposSN=ccoros$corotiposSN, ncor=ccoros$ncor,
                CorElementos=dis_elcor, desCorotipos=desCorotipos,
                ldifusa=ldifusa, mcardinales=mcardinales, mentropia=mentropia, isprox=isprox)
#                mCorLoc=mCorLoc)

    class(macoquires) <- "RMacoqui"

##########################################################################################################
#
#
#                                           Imprimimos los resultados:
#
#
##########################################################################################################

if (lang=="es") {
      msg1 <-  paste(" Valores iniciales: \n ================== \n     Metodo                    " , macoquires$metodo,"\n",
                     "    Numero de especies:       ", macoquires$nvar,"\n" ,"    Numero de localizaciones: ", macoquires$nval, "\n\n" ,
                     "Valores criticos: \n ================= \n")
      msg2 <- paste("\nMatriz de similitudes: Orden Inicial: \n=====================================\n\n")
      msg3 <- paste("\nMatriz de similitudes: Orden segun el dendrograma:\n==================================================  \n\n")
      msg4 <- paste("\n\nMatriz de similitudes significativas:\n=====================================\n\n")
      msg5 <- paste(" \nLocalizacion de corotipos en el dendrograma:\n============================================\n",
                    "Similitudes y parametros intermedios\n\n")
      msg6 <- paste("\n\n\nParametros de MACOQUI:\n======================\n\n")
    }
    else {
      msg1 <-  paste(" Initial    Values: \n ================== \n     Method                    " , macoquires$metodo,"\n",
                     "    Number  of species:       ", macoquires$nvar,"\n" ,"    Number   of  localities:  ", macoquires$nval, "\n\n" ,
                     "Critical  Values \n ================ \n" )
      msg2 <- paste("\nSimilarity matrix: Initial arrangement \n======================================  \n\n")
      msg3 <- paste("\nSimilarity matrix: Arrangement according to the dendrogram \n==========================================================  \n\n")
      msg4 <- paste("\n\nSignificant similarity matrix \n=============================\n\n")
      msg5 <- paste(" \nChorotype search in the dendrogram \n==================================\n",
                    " Similarities and intermediate parameters \n\n")
      msg6 <- paste("\n\n\nMACOQUI parameters (not null) \n=============================\n\n")
    }

    cat (msg1)
    print( macoquires$extremos)

    cat (msg2)
    print (round(macoquires$mprox, digits=3))

    cat( msg3)
    print (round(macoquires$mproxOrdc, digits=3))

#    cat( " \nMatriz de signos: \n")
    cat( msg4)
    print (macoquires$msignosC)

#    cat( " \nMatriz de valores Macoqui: \n")
    cat( msg5)
    sim<-1-clres$cd
    macoquires$macdatos<-cbind(sim, macoquires$macdatos)
    print (round(macoquires$macdatos, digits=3))

    cat( msg6)
    print (round(macoquires$macres[rowSums(is.na(macoquires$macres[,7:18]))!=12,], digits=3))

#
# LOGICA DIFUSA:  Solo en caso de metodo = BARONI   y si tenemos corotipos
#
if (lang=="es") {
      msg1 <- paste(" \n\nCOROTIPOS \n========= \n  Numero de Corotipos:                         ", macoquires$ncor,"\n\n")
}
else {
      msg1 <- paste(" \n\nCHOROTYPES \n========== \n  Number of Chorotypes:                    ", macoquires$ncor,"\n\n")
}
cat(msg1)

if (ncor > 0 & metodo == "baroni") {
    if (lang=="es") {
      msg2a <- paste( " \n\nLogica Difusa:\n==============\n")
      msg2b <- paste( " \n\nIntersecciones y uniones entre corotipos\n\n")
      msg3 <- paste("\n\nCardinal Interseccion\\Union:\n=============================  \n\n")
      msg4 <- paste(" \n\nParametros corotipos difusos:\n=============================\n\n")
      msg5 <- paste(" \n\nInclusion:\n==========\n\n")
      msg6 <- paste("\n\nSuperposicion Borrosa:\n======================\n\n")
    }
    else {
      msg2a <- paste( " \n\nFuzzy Logic\n===========\n")
      msg2b <- paste( " \n\nIntersection and union between chorotypes\n\n")
      msg3 <- paste("\n\nCardinality Intersection\\Union\n===============================  \n\n")
      msg4 <- paste(" \n\nChorotype fuzzy parameters\n==========================\n\n")
      msg5 <- paste(" \n\nInclusion:\n==========\n\n")
      msg6 <- paste("\n\n\nFuzzy Overlap  \n=============\n\n")
    }


    print(desCorotipos)
    cat("\n\n")
    cat(msg2a)
    print(round(ordenaFilas(macoquires$CorElementos, c(clres$iord, nrow(macoquires$CorElementos))),digits=3))


#    
# Si ncor=1 no hay intersecciones/uniones: No calculo de entropia, cardinales   
#
    if (ncor>1) {
        cat(msg2b)
        print (round(cbind(macoquires$ldifusa[,1:2],macoquires$ldifusa[,(((macoquires$ncor)*4 +3):(ncol(macoquires$ldifusa)))]), digits=3))
    
        cat(msg3)
        print (round(macoquires$mcardinales, digits=3))
    
        cat(msg4)
        print (round(macoquires$mentropia[,1:4], digits=3))
    
        cat(msg5)
        print (round(macoquires$mentropia[,5:(4+macoquires$ncor)], digits=3))
    
        cat(msg6)
        print (round(macoquires$mentropia[,(5+macoquires$ncor):ncol(macoquires$mentropia)], digits=3))
    }

}
    cat("\n\n\n")

    cat(ifelse(lang=="es","Finalizado: ","End"), date(),"\n")

    macoquires[["datos"]] <- datos
    invisible (macoquires)
#   return( cat("Finalizado: ", date(),"\n"))


 }
