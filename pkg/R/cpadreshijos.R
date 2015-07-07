cpadreshijos <-
function (etprevial, etpreviau) {
      nvar<-length(etprevial)+1
      PadresHijos    <- array(0, dim=c(nvar-1, nvar-1))
      respadreshijos <-.Fortran("CalculaPadresHijos", as.integer(nvar),  
                          as.integer(etprevial), as.integer(etpreviau),
                          as.integer(PadresHijos))
                          
      return(matrix(unlist(respadreshijos[4]), ncol=nvar-1))                    
    }
