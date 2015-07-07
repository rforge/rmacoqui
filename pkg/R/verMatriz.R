verMatriz <-
function(matriz,title="")
{
# etiquetas de botones: Dependiendo del idioma es o en

 etbt2Excel        <-  "Save Excel"     
 etbtOrdenClus     <-  "Cluster Order"     
 etbtOrdenIni      <-  "Initial Order"  
 etbt2Txt          <-  "Save txt" 

   if (lang=="es" ) 
     { etbt2Txt      <- "Guardar txt"
       etbt2Excel    <- "Guardar Excel"
       etbtOrdenClus <- "Orden Cluster"
       etbtOrdenIni  <- "Orden Inicial" 
     }
 
  altura  <-  nrow(matriz)
  anchura <-  ncol(matriz)
  
  fres<-paste(gsub(" ","_",title),".txt")
  

  if (is.null(dimnames(matriz))) {
    dimnames(matriz)<-list(paste("Fila",1:altura),paste("Col",1:anchura))
  }

#  require(tcltk2)

  tclArray <- tclArrayVar(matriz)

  tt <- tktoplevel()
  tclRequire("Tktable")
  tclArrayName <- ls(tclArray$env)

##  tkwm.title(tt,tclArrayName)
  tkwm.title(tt,title)

# Funciones asociadas a los botones

salvarTxt <- function () {
    txtsalvado    <- "The file has been saved: "
    bttxt  <- "Save file"
    if (lang == "es" ) then 
      { txtsalvado   <- "Se ha salvado el fichero: "
        bttxt <- "Salvar Fichero"
      }
    
# Separados por comas
    write.csv(matriz, fres)
    mensaje <- paste(txtsalvado, getwd(),"/",fres)
    ReturnVal <- tkmessageBox(title=bttxt, message=mensaje,icon="info",type="ok")

  }

# Botones

#tkconfigure(btOrdenClus, btOrdenIni)
#tkgrid(btOrdenClus, btOrdenIni)
#  tkgrid(bt2Excel,bt2Txt,btOrdenClus,btOrdenIni)

 txtncol <- tklabel(tt,text=paste("Cols: ", anchura))
 txtnfil <- tklabel(tt,text=paste("Rows: ", altura))
 txtsep  <- tklabel(tt,text="  -  ")
 tkgrid(txtncol,  sticky="w")



 bt2Txt  <-  tkbutton(tt, text=etbt2Txt, command=salvarTxt)
 tkgrid(txtnfil, bt2Txt, sticky="w")

 tkgrid(tklabel(tt,text="    "))

#################################################

  table1 <- tkwidget(tt,"table",rows=paste(tclArray$nrow+1),cols=paste(tclArray$ncol+1),
                         titlerows="1",titlecols="1", colwidth="12",
                         height="0", width="0",
                         xscrollcommand=function(...) tkset(xscr,...),
                         yscrollcommand=function(...) tkset(yscr,...))

  xscr <- tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))
#  tkcmd(.Tk.ID(table1),"width","0","10")

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")

  tkconfigure(table1,variable=tclArrayName,background="white",selectmode="extended", rowseparator="\"\n\"",colseparator="\"\t\"")

}
