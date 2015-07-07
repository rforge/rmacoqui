gruposcorrectos <-
function (grupos, nvar) { 
    error      <- 0   
    msgerror   <- ""
    ngrupos    <- length(grupos)/2                # debe ser par

#Comprobacion de n?mero de correcto de pares  : %% Obtiene el m?dulo
    if (length(grupos)%%2 == 1) error <- 1
    
#Comprobacion de nodo y rama
    if (error == 0) {
      for (i in (1:ngrupos)) {
        if (!(grupos[2*i] == 1 || grupos[2*i] == 2)) {
            error <- 2
            msgerror <- paste("Error en pos. ",2*i, " Para una rama, el valor deber ser 1 o 2")
        }
        if ((grupos[2*i-1] < 1  || grupos[2*i-1] > nvar-1)) {
            error <- 3
            msgerror <- paste("Error en nodo: ",grupos[2*i-1], " Nodo inexistente")
        }
        if (error != 0 ) break
      }
  }
# Nodos repetidos
    if (error == 0)
      for (i in (1:(ngrupos-1)))
        for (j in (i+1):ngrupos)
          if (grupos[2*i-1] == grupos[2*j-1]) {
            error <-4
            break
          }

  return (error)
}
