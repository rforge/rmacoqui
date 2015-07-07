print.tclArrayVar <-
function(tclArray,matriz,height=-1,width=-1,title="")
{
  require(tcltk2)
  tt <- tktoplevel()
  tclRequire("Tktable")
  tclArrayName <- ls(tclArray$env)
##  tkwm.title(tt,tclArrayName)
  tkwm.title(tt,title)
  
# Funciones asociadas a los botones

  toExcel <- function () {
      write.xls(matriz, "fexcel.xls")
  }

  toTxt <- function () {
# Separados por comas
    write.csv(matriz, "ftexto.txt")
  }
  
# Botones
 bt2Excel        <-  tkbutton(tt, text=etbt2Excel ,command=toExcel)     
 bt2Txt          <-  tkbutton(tt, text=etbt2Txt,  command=toTxt)    

 btOrdenClus     <-  tkbutton(tt, text=etbtOrdenClus, command=OrdClus)     
 btOrdenIni      <-  tkbutton(tt, text=etbtOrdenIni, command=OrdIni)    
 
#tkconfigure(btOrdenClus, btOrdenIni)
#tkgrid(btOrdenClus, btOrdenIni)
#  tkgrid(bt2Excel,bt2Txt,btOrdenClus,btOrdenIni)
  
 tkgrid(bt2Excel, sticky="e")
 tkgrid(bt2Txt, sticky="e")
 
 tkgrid(btOrdenClus, sticky="e")
 tkgrid(btOrdenIni, sticky="e")
 tkgrid(tklabel(tt,text="    "))
#################################################  

  table1 <- tkwidget(tt,"table",rows=paste(tclArray$nrow+1),cols=paste(tclArray$ncol+1),
                         titlerows="1",titlecols="1",
                         height=paste(height+1), width=paste(width+1),
                         xscrollcommand=function(...) tkset(xscr,...),
                         yscrollcommand=function(...) tkset(yscr,...))
                         
  xscr <- tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
  yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))

  tkgrid(table1,yscr)
  tkgrid.configure(yscr,sticky="nsw")
  tkgrid(xscr,sticky="new")

  tkconfigure(table1,variable=tclArrayName,background="white",selectmode="extended", rowseparator="\"\n\"",colseparator="\"\t\"")

}
