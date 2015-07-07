significacion <-
function(x) {
    aux<-""
#
#' Dado x, supuesto valor calculado de una ji-cuadrado de 1 grado de libertad,
#' devuelve si es significativo o no. Si devuelve
#'  0  ns    : no es significativo: p>0.05
#'  1   *    : 0.05  >= p  > 0.01
#'  2  **    : 0.01  >= p  > 0.001
#'  3 ***    : 0.001 >= p
#'
#' Nota: x, como valor de una ji-cuadrado, debe ser positivo
#
      aux  <- ifelse(x < 3.841,"n.s.", 
              ifelse((x > 3.84) & (x < 6.635), "*", 
              ifelse((x > 6.634) & (x < 10.828), "**", "***")))

      return(aux)
 }
