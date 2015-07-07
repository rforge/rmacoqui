cmatrizsignos <-
function(mbaroni, vmin, vmax, tipodatos=2, ifail=-1)
    {  nvar<-ncol(mbaroni)
       msignos <- array(0, dim=c(nvar, nvar))
       resmatrizsignos =.Fortran("calculamsignos",
                                     as.integer(nvar), as.double(vmax), as.double(vmin), 
                                     as.integer(msignos), as.double(mbaroni), 
                                     as.integer(tipodatos), as.integer(ifail))
                                     
       msignos<-matrix(unlist(resmatrizsignos[4]), ncol=nvar)
       dimnames(msignos)<-dimnames(mbaroni)
       return(msignos)
    }
