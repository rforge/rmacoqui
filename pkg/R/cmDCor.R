cmDCor <-
function (ncorotipos, cor_ele, mprox) {
             ncor   <- ncorotipos   
             nvar   <- length(cor_ele)
             mdcor  <- array(0, dim=c(nvar+1,ncor))
             
             dcor_ele  <- .Fortran("cMDCor", as.integer(nvar),as.integer(ncor), 
                              as.integer(cor_ele), as.double(mprox), as.double(mdcor))

             dis_elcor   <- unlist(dcor_ele[5])
             
             elc<-cbind(seq(1:nvar), cor_ele)

             
             elc<-rbind(elc,c(0,0))
             elc<-cbind(elc, matrix(dis_elcor, ncol=ncor))

             if (lang=="es") {             
                 etcol<-  c("Especie", "Corotipo", paste("Pertenencia C",1:ncorotipos,sep="") )
#                 if (npsg>0) etcol <- c(etcol,  paste("Pertenencia G",1:npsg,sep=""))
             } 
             else {
                 etcol<-  c("Species", "Chorotype", paste("Membership C",1:ncorotipos,sep="") )
#                 if (npsg>0) etcol <- c(etcol,  paste("Membership G",1:npsg,sep=""))
             }

######             dimnames(elc)<-list(c(seq(1:nvar),"Cardinal"), etcol)
             dimnames(elc)<-list(c(rownames(mprox),"Cardinal"), etcol)
             
#                    c("Elemento", "Corotipo", paste("Grado Pertenencia C",1:ncor, sep="")))
#                    c("Especie", "Corotipo", c(paste("Pertenencia C",1:ncorotipos,sep=""), paste("Pertenencia G",1:npsg,sep=""))))
##                    c("Specie", "Chorotype", paste("Membership C",1:ncor, sep="")))


             return(elc)
}
