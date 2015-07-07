xposnodos <-
function(orden, mezcla) {
    ne <- nrow(mezcla)    # netapas
    nl <- length(orden)   # nelementos
    x<- seq(1:ne)
    for (i in 1:ne) {

          if (mezcla[i,1] < 0 & mezcla[i,2] < 0) {
            for (k in 1:nl) {
                if (-mezcla[i,1] == orden[k] ) k1 <- k
                if (-mezcla[i,2] == orden[k] ) k2 <- k
            }
          }
 
          if (mezcla[i,1] < 0 & mezcla[i,2] > 0) {
            for (k in 1:nl) { if (-mezcla[i,1] == orden[k]) k1 <- k }
            k2 <-  x[mezcla[i,2]]
          }

          if (mezcla[i,1] > 0 & mezcla[i,2] < 0) {
              for (k in 1:nl) { if (-mezcla[i,2] == orden[k]) k1 <- k }
              k2 <- x[mezcla[i,1]]
          }

          if (mezcla[i,1] > 0 & mezcla[i,2] > 0) {
              k1 <- x[mezcla[i,1]] 
              k2 <- x[mezcla[i,2]]
    }
    x[i] <- (k1+k2)/2
    }
    return (x)
}
