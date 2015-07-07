cmsignos <-
function(mprox, datos, extremos, metodo, tipodatos=2, ifail=-1) { 
    nvar<-ncol(mprox)
    msignos <- array(-9, dim=c(nvar, nvar))
    if (metodo=="baroni") {
        #            extremos    <- cextremos(nval)
        resmatrizsignos =.Fortran("calculamsignos",
                        as.integer(nvar), as.double(extremos$vmax), as.double(extremos$vmin), 
                        as.integer(msignos), as.double(mprox), 
                        as.integer(tipodatos), as.integer(ifail))
                           
        msignos<-matrix(unlist(resmatrizsignos[4]), ncol=nvar)
        #            dimnames(msignos)<-dimnames(mprox)
    }
    ## Matrices de Correlacion 
    if(metodo=="pearson" | metodo=="kendall" | metodo=="spearman") {
      for (i in 1:nvar) for (j in 1:nvar) {
          ct<-cor.test(datos[,i], datos[,j], method=metodo)
          x<-ct$p.value
          msignos[i,j] <- sign(ct$estimate)*ifelse(x<0.001,3,ifelse(x<0.01,2,ifelse(x<0.05,1,0)))
      }                      
    }
    dimnames(msignos)<-dimnames(mprox)
    return(msignos)
    
}
