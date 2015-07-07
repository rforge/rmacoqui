ver.matRmacoqui <-
function (x) {

# El parametros de entrada no es correcto:

## if (length(x) < 5 | !is(x)=="RMacoqui" | !is(x)=="RFuzzy" ) 
##   return ("El par?metro debe ser un objeto RMacoqui o RFuzzy")

if (is(x) == "RMacoqui")  {
      nmat <- 11     # Numero de matrices a mostrar
      tt<-tktoplevel()
      tkwm.title(tt,"M A C O Q U I   Mats")

      scr <- tkscrollbar(tt, repeatinterval=5,
      				   command=function(...)tkyview(tl,...))
      tl<-tklistbox(tt,height=12,width="40",selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
      tkgrid(tklabel(tt,text="Matrix RMacoqui"))
      tkgrid(tl,scr)
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      if (lang=="es") {
        mats <- c("Similitud - Orden Inicial","Similitud - Orden Cluster", "Similitud Significativa",
                "Parametros intermedios de MACOQUI", "Parametros MACOQUI", "Descripcion Corotipos", "Grado de pertenencia a Corotipos",
                "Parametros intermetidos de Logica Difusa", "Cardinalidad Interseccion-Union entre Corotipos", "Parametros de Logica Difusa", "Corotipos en Localidades")
        etbtOK <- "   Ver   "      }
      else {
        mats <- c("Similarity Initial Order", "Similarity Cluster Order", "Significant Similarity",
                "Intermediate MACOQUI Parameters", "MACOQUI Parameters", "Chorotype Report", "Degree of Membership in Chorotypes",
                "Intermediate Fuzzy Parameters", "Cardinality Intersection-Union between Chorotypes", "Fuzzy Parameters", "Chorotypes in Localities")
        etbtOK <- "  View   "      }
        
      for (i in (1:nmat))  tkinsert(tl,"end",mats[i])
      tkselection.set(tl,1)

      OnVer <- function()
      {
        	matelegida <- paste("Macoqui_" , mats[as.numeric(tkcurselection(tl))+1])
          if (as.numeric(tkcurselection(tl)) == 0 )    verMatriz(round(x$mprox, digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 1 )    verMatriz(round(x$mproxOrdc, digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 2 )    verMatriz(x$msignosC, title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 3 )    verMatriz(round(x$macdatos,digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 4 )    verMatriz(round(x$macres,digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 5 )    verMatriz(x$desCorotipos, title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 6 )    verMatriz(round(x$CorElementos,digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 7 )    verMatriz(round(x$ldifusa,digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 8 )    verMatriz(round(x$mcardinales,digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 9 )    verMatriz(round(x$mentropia,digits=3), title=matelegida)
          if (as.numeric(tkcurselection(tl)) == 10)    {
            if (is.null(x$locCorotipos)){
                print ("Undefined Chorotypes in Localities")
            } else{
                verMatriz(round(x$locCorotipos,digits=3), title=matelegida)
            } 
            
          }
      }
          OK.but <-tkbutton(tt, text=etbtOK, command=OnVer)
          tkgrid(OK.but)
          tkfocus(tt)

}  else if (is(x) == "RFuzzy") {
      nmat <- 7     # N?mero de matrices a mostrar

      tt<-tktoplevel()
      tkwm.title(tt,"Fuzzy-Clusters Mats")

      scr <- tkscrollbar(tt, repeatinterval=5,
      				   command=function(...)tkyview(tl,...))
      tl<-tklistbox(tt,height=12,width="50",selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white")
      tkgrid(tklabel(tt,text="Matrix RMacoqui"))
      tkgrid(tl,scr)
      tkgrid.configure(scr,rowspan=4,sticky="nsw")
      if (lang=="es") {
        mats <- c("Descripcion Grupos", "Pertenencia a grupos", "Grado Pertenencia a grupos",
                "Logica Difusa Grupos", "Cardinalidad Interseccion-Union entre Grupos",
                "Parametros de Logica Difusa", "Grupos en Localidades")
        etbtOK <- "   Ver   "        }
      else {
        mats <- c("Cluster Description ", "Species Presences in Clusters", "Degree of Membership in Clusters",
                "Intermediate Fuzzy Parameters", "Cardinality Intersection-Union between Clusters",
                "Fuzzy Parameters", "Clusters in Localities")
        etbtOK <- "  View   "        }
      for (i in (1:nmat)) tkinsert(tl,"end",mats[i])
      tkselection.set(tl,1)

      OnVerf <- function()
      {
          	matelegida <- paste("FuzzyClusters_", mats[as.numeric(tkcurselection(tl))+1])
            if (as.numeric(tkcurselection(tl)) == 0 )    verMatriz(x$desGrupos, title=matelegida)
            if (as.numeric(tkcurselection(tl)) == 1 )    verMatriz(round(x$elementosGrupos1, digits=3), title=matelegida)
            if (as.numeric(tkcurselection(tl)) == 2 )    verMatriz(round(x$elementosGrupos2,digits=3), title=matelegida)
          ##   if (as.numeric(tkcurselection(tl)) == 3 )    verMatriz(round(x$maclistas,digits=3), title=matelegida) --> Parametros intermedios
            if (as.numeric(tkcurselection(tl)) == 3 )    verMatriz(round(x$ldifusaGrupos,digits=3), title=matelegida)
            if (as.numeric(tkcurselection(tl)) == 4 )    verMatriz(round(x$mcardinales,digits=3), title=matelegida)
            if (as.numeric(tkcurselection(tl)) == 5 )    verMatriz(round(x$mentropia, digits=3),title=matelegida)
            if (as.numeric(tkcurselection(tl)) == 6 )    verMatriz(round(x$locGrupos,digits=3), title=matelegida)
          ##  if (as.numeric(tkcurselection(tl)) == 10)    verMatriz(round(x$mCorLoc,digits=3), title=matelegida)} --> Desc. Corotipos
      }
            OK.but <-tkbutton(tt, text=etbtOK, command=OnVerf)
            tkgrid(OK.but)
            tkfocus(tt)
}

}
