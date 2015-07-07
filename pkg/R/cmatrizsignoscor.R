cmatrizsignoscor <-
function(datos,metodo="pearson") { 
      nvar    <-ncol(datos)
      msignos <-array(-9, dim=c(nvar, nvar))
      for (i in 1:nvar) for (j in 1:nvar) {
          ct<-cor.test(datos[,i], datos[,j], method=metodo)
          x<-ct$p.value
          msignos[i,j] <- sign(ct$estimate)*ifelse(x<0.001,3,ifelse(x<0.01,2,ifelse(x<0.05,1,0)))
      }                   
      return (msignos)
    }
