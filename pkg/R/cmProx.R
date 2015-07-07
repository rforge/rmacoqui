cmProx <-
function (datos, metodo) {
              if(!( metodo=="baroni" | metodo=="jaccard" | metodo=="pearson" | metodo=="kendall" | metodo=="spearman")) return(cat("El metodo ", metodo," no est? implementado.\n"))
              
              if (metodo=="baroni") {
                nvar<- ncol(datos)
                nval<- nrow(datos)
                nvariables<-seq(1:nvar)
                
                if(is.list(datos)) {
                   auxdatos<- matrix(unlist(datos), ncol=nvar)
                   nvariables<-paste(1:nvar,names(datos))
                   }
            
                if(is.matrix(datos)) auxdatos<-datos
              
                mbaroni <- array(0, dim=c(nvar, nvar))
                resul =.Fortran("calculambaroni",
                         as.double(auxdatos), as.integer(nrow(datos)),
                         as.integer(nvar),mbaroni=as.double(mbaroni), as.integer(nval))
                mbaroni <-  matrix(unlist(resul$mbaroni), ncol=nvar)
                dimnames(mbaroni)<-list(nvariables,c(1:nvar))
                mprox <- mbaroni
              }
              
              if (metodo=="jaccard") { 
                   return(cat("No implementado \n"))
              }
                                           
              if (metodo=="pearson" | metodo=="kendall" | metodo=="spearman") {
                       mprox<-cor(datos, method=metodo)
              }
              
              return(mprox)
      }
