mbar2mprox <-
function(mbaroni, tmatriz){
     nvar<-ncol(mbaroni)
     ifail <- -1
     mdist <- array(0, dim=c(nvar*(nvar-1)/2))
     resul2 =.Fortran("mbaroniamprox", 
                      as.double(mbaroni),as.double(mdist), as.integer(nvar), 
                      as.integer(tmatriz), as.integer(ifail))

     return(matrix(unlist(resul2[2])))
     }
