mprox2mclus <-
function(mprox, tmatriz){
     nvar<-ncol(mprox)
     ifail <- -1
     mclus <- array(0, dim=c(nvar*(nvar-1)/2))
     resul2 =.Fortran("mprox2mclus", 
                      as.double(mprox), as.double(mclus), as.integer(nvar), 
                      as.integer(tmatriz), as.integer(ifail))
     return(matrix(unlist(resul2[2])))
     }
