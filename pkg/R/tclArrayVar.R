tclArrayVar <-
function(Rarray=NULL)
{
  if (!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray))!=2)
    stop("Array must be one-dimensional or two-dimensional.")
  require(tcltk)
  
  n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount +
    1L
  name <- paste0("::RTcl", n)
  l <- list(env = new.env())
  assign(name, NULL, envir = l$env)
  reg.finalizer(l$env, function(env) tcl("unset", ls(env))) 
  class(l) <- "tclVar"

  
  if (is.null(Rarray))
  {
    ndim <- 2
    .Tcl(paste("set ",name,"(0,0) \"\"",sep=""))
  }
  else
  {
    if (is.vector(Rarray))
    {
      ndim <- 1
      Rarray <- as.data.frame(Rarray)
    }
    else
      ndim <- 2
    for (i in (1:nrow(Rarray)))                          
      if (ndim==2)
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",j,") \"",paste(Rarray[i,j]),"\"",sep=""))
    else
      .Tcl(paste("set ",name,"(",i,",",1,") \"",paste(Rarray[i,1]),"\"",sep=""))
    if (!is.null(rownames(Rarray)))
      for (i in (1:nrow(Rarray)))
        .Tcl(paste("set ",name,"(",i,",",0,") \"",rownames(Rarray)[i],"\"",sep=""))
    else
      for (i in (1:nrow(Rarray)))
        .Tcl(paste("set ",name,"(",i,",",0,") \"\"",sep=""))
    if (!is.null(colnames(Rarray)))
      for (j in (1:ncol(Rarray)))
        .Tcl(paste("set ",name,"(",0,",",j,") \"",colnames(Rarray)[j],"\"",sep=""))
    else
      for (j in (1:ncol(Rarray)))
        .Tcl(paste("set ",name,"(",0,",",j,") \"\"",sep=""))
    l$nrow <- nrow(Rarray)
    l$ncol <- ncol(Rarray)
  }
  l$ndim <- ndim
  l
}
