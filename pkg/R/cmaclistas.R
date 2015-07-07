cmaclistas <-
function(ilc, iuc){
    nvar<-length(ilc)+1
    etprevial      <- array(0, dim=c(nvar-1))
    etpreviau      <- array(0, dim=c(nvar-1))
    macoquilistas  <- array(0, dim=c(nvar-1, nvar))   
    
    poblaciones =.Fortran("poblaciones", 
                        as.integer(nvar), as.integer(ilc), as.integer(iuc),
                        etprevial=as.integer(etprevial), etpreviau=as.integer(etpreviau),
                        maclistas=as.integer(macoquilistas))
#    maclistas<-matrix(unlist(poblaciones[6]), ncol=nvar)
    return (poblaciones[4:6])
    }
