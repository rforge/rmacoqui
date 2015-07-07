maccluster <-
function(mprox, tmatriz=1, metodoclust=3, ifail=-1){
  nvar <- ncol(mprox)
  ilc  <- array(0, dim=c(nvar-1));  iuc <- array(0, dim=c(nvar-1));  
  iwk  <- array(0, dim=c(2*nvar)); iord <- array(0, dim=c(nvar));
  cd   <- array(0, dim=c(nvar-1)); dord <- array(0, dim=c(nvar));
  mdist<-mprox2mclus(mprox, tmatriz)
   
  ccluster = .Fortran("macocluster", as.integer(metodoclust), as.integer(nvar),
                                       as.double(mdist),ilc=as.integer(ilc),
                                       iuc=as.integer(iuc),cd=as.double(cd), 
                                       iord=as.integer(iord),dord=as.double(dord), 
                                       as.integer(iwk), ifail=as.integer(ifail))

  dc<-adendrograma(ccluster$ilc,ccluster$iuc, ccluster$iord, ccluster$cd) 
  dc$labels <- dimnames(mprox)[[1]]
  x<- xposnodos(dc$order, dc$merge)
  plot(dc, ylab="Dissimilarity", xlab="", sub="")
#  plot(dc, ylab="Dissimilarity", xlab="", sub="", hang=0)

  text(x,dc$height, 1:length(dc$height), col="blue")
     
  return (list(ilc=ccluster$ilc,iuc=ccluster$iuc,cd=ccluster$cd,
                     dord=ccluster$dord,iord=ccluster$iord, dend=dc,x=x))   
  }
