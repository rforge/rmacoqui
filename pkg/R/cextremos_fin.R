cextremos_fin <-
function(nval) {
            vmax<-0
            vmin<-0 
# Si NVAL > 500 
            if (nval>500) {
              cat (" The number of attributes (sites, grids, localities) is higher than 500. \n");
              cat (" Critical values for the distribution of similarities are not \n"); 
              cat ("  available for N>500 (see Baroni-Urbani & Buser, 1976 \n");
              cat (" [Similarity of binary data, Syst. Zool. 25:251-259]).\n\n");
              cat (" Two options are proposed:  \n");
              cat ("1: Reshape the data matrix in order to make N be 500 or lower\n"  );
              cat ("2: Chose other critical values\n");
              r1 <- as.numeric (readline("Write 1 or 2 and press Enter:  "));
              if (r1 == 2) {
                cat (" You are proposed two possibilities:  \n");
                cat ("1: Use the critical values for N=500\n"  );
                cat ("2: Chose critical values manually\n");
                r2 <- as.numeric (readline("Write 1 or 2 and press Enter:  "));
                if (r2 == 2) {
                  vmax <- as.numeric(readline("Write the critical value for highly significant similarities '+' (for example 0.530)  and press Enter:  "));
                  vmin <- as.numeric(readline("Write the critical value for highly significant disimilarities '-' (for example 0.470) and press Enter: "));
                } 
                else {
                  nval<-500                             
                  valores= .Fortran("extremosbaroni",as.integer(nval), 
                                     vmax=as.double(vmax), vmin=as.double(vmin)) 
                  vmin <- valores$vmin;
                  vmax <- valores$vmax;
                }
              }
              else {
                 cat (" xxx ha elegido  1.	Reshape the data matrix in order to make N be 500 or lower\n");
              }   
# FIN CASO DE NVAL > 500    
            }
            else {
              valores= .Fortran("extremosbaroni",as.integer(nval), 
                                     vmax=as.double(vmax), vmin=as.double(vmin)) 
              vmin <- valores$vmin;
              vmax <- valores$vmax;
            }                               
                                   
            return(c(vmin,vmax))
            }
