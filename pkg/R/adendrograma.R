adendrograma <-
function(ilc, iuc, iord, dord){
                   n<-length(iuc);
                   g<-array(0, dim=c(n));
                   mi<-array(0, dim=c(n,2));
                   for (i in 1:n) {g[i]<-ifelse(ilc[i]<iuc[i], ilc[i], iuc[i])};
                   mi[1,1]<- -ilc[1]; mi[1,2]<- -iuc[1];
                   for (i in 2:n) {
                       mi[i,1]=-ilc[i]; mi[i,2]=-iuc[i];
                       for (j in (i-1):1) {if (ilc[i] == g[j]) {mi[i,1]<-j; break} }            
                       for (j in (i-1):1) {if (iuc[i] == g[j]) {mi[i,2]<-j; break} }            
                   };
                   tree <- list(merge = mi[,1:2],
                                height = dord,
                                order  = iord,
                                labels = NULL,
                                method = NULL,
                                call   = match.call(),
                                dist.method = "Macoqui")
                   class(tree) <- "hclust"
                   return(tree)
               }
