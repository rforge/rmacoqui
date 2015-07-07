mProx_valida <-
function (datos) {
  
  es_mprox <- 0
  matriz <- as.matrix(datos)        # La convertimos a matriz
  
  
  
  if (is.matrix (matriz)) {
    if (ncol(matriz) == nrow(matriz)) {                                 # Debe ser cuadrada
      if  (sum(diag(matriz)) == nrow(matriz)) {                       # 1's en la diag principal  
        for (i in (1:(nrow(matriz)-1))) {                          # es sim?trica
          for (j in ((i+1):nrow(matriz))) {
            if(matriz [i,j] == matriz [j,i]) es_mprox <- 1
            if (!es_mprox) break             
          }
          if (!es_mprox) break                                 
        }              
      }
    }
  } 
  ## ok. Es una matriz chachi de proximidades. Le ponemos las etiquetas.
  if (es_mprox) {
    nombres         <- paste(1:ncol(matriz), colnames(matriz))  
    dimnames(matriz) <-list(nombres,c(1:ncol(matriz)))
  }
  
  
  return (list(mprox_ok = es_mprox, mprox = matriz))
  
}
