elecorotipos <-
function (macoquilistas, corotiposSN){
      nvar <-  ncol(corotiposSN) + 1
      corotipos_ele <- array(0, dim=c(nvar))

# Para evitar los psg (corotiposSN=-2) con los que se trabaja en la funcion corotiposelemtos,
# substituimos dicho valor por -22
# Hay que recompilar rutinas_macoqui.f
#
      corotiposSNaux <- corotiposSN
      for (i in 1:(nvar-1)) {
        if (corotiposSNaux[1,i] == -2) {corotiposSNaux[1,i] <- -22}
        if (corotiposSNaux[2,i] == -2) {corotiposSNaux[2,i] <- -22}
        
      }

      cor_ele  <- .Fortran("corotiposelementos", as.integer(nvar), as.integer(macoquilistas),
                        as.integer(corotiposSNaux), as.integer(corotipos_ele))

#      corotipos_ele<-rbind(seq(1:nvar),unlist(cor_ele[4]))                        
#      return(corotipos_ele)
      return(unlist(cor_ele[4]))
    }
