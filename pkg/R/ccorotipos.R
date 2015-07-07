ccorotipos <-
function (etprevial, etpreviau, vdwa, vdwb, vgwa, vgwb){
      nvar <- length(etprevial)+1
      ncor <- 0
      corotiposSN   <- array(-9, dim=c(nvar-1,2))
      PadresHijos   <- cpadreshijos(etprevial, etpreviau)
      rescorotipos  <- .Fortran("Corotipos", as.integer(nvar), as.integer(corotiposSN), 
                        as.double(vdwa), as.double(vdwb), as.double(vgwa), as.double(vgwb),
                        as.integer(PadresHijos), as.integer(ncor))
                        
      corotiposSN   <-  matrix(unlist(rescorotipos[2]), ncol=nvar-1, byrow=TRUE)  
      return(list(corotiposSN=corotiposSN, ncor=unlist(rescorotipos[8])))
    }
