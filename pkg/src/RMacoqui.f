      SUBROUTINE calculambaroni (xdatos, nfil, ncol, mbaroni, nval)
      integer nfil, ncol, nval
      double precision xdatos(nfil,ncol), x(ncol),  mbaroni(ncol, ncol)
      double precision cc, dd, rad, bar_ij
      DOUBLE PRECISION ab(ncol), c(ncol, ncol), d(ncol, ncol)
      integer i, j, k

C Inicializamos las variables
      nval=0
      cc=0
      dd=0
      rad=0
      bar_ij=0

      do i=1, ncol
         ab(i)=0
         x(i)=0
         do j=1, ncol
            c(i,j)=0
             d(i,j)=0
         enddo
      enddo

C Inicio de proceso
      do j=1, nfil
        nval=nval+1
        do i = 1, ncol
            x(i)= xdatos(j, i)
            AB(i) = AB(i) + x(i)
            do k = i + 1,  ncol
                x(k)= xdatos(j, k)
                If (x(i) + x(k) .EQ. 2.) Then
                   C(i, k) = C(i, k) + 1
                endif
                If (x(i) + x(k) .eq. 0.) Then
                   d(i, k) = d(i, k) + 1
                endif
            enddo
        enddo
      enddo
C
C  Matriz de Baroni
C
      do i = 1,ncol
         mbaroni(i, i) = 1
         do j = i + 1, ncol
            cc = C(i, j)
            dd = d(i, j)
            RAD = cc * dd
            BAR_IJ = ((Sqrt(RAD) + C(i, j)) /
     c               (Sqrt(RAD) + AB(i) + AB(j) - C(i, j)))
            mbaroni(i, j) = BAR_IJ
            mbaroni(j, i) = BAR_IJ
         enddo
      enddo

      end

CCccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C   Calcula matriz de proximidades: Jaccard o Baroni
C
C  tmatriz ¡ndica el tipo de matriz de vamos a obtener
C      =1  Matriz de Baroni
C      =2  Matriz de Jaccard
C

      SUBROUTINE calculaMProx(xdatos, nfil, ncol, mprox, nval, tmatriz)
      integer nfil, ncol, nval, tmatriz
      double precision xdatos(nfil,ncol), x(ncol),  mprox(ncol, ncol)
      double precision cc, dd, rad, bar_ij
      DOUBLE PRECISION ab(ncol), c(ncol, ncol), d(ncol, ncol)
      integer i, j, k

C Inicializamos las variables
      nval=0
      cc=0
      dd=0
      rad=0
      bar_ij=0

      do i=1, ncol
         ab(i)=0
         x(i)=0
         do j=1, ncol
            c(i,j)=0
             d(i,j)=0
         enddo
      enddo

C Inicio de proceso
      do j=1, nfil
        nval=nval+1
        do i = 1, ncol
            x(i)= xdatos(j, i)
            AB(i) = AB(i) + x(i)
            do k = i + 1,  ncol
                x(k)= xdatos(j, k)
                If (x(i) + x(k) .EQ. 2.) Then
                   C(i, k) = C(i, k) + 1
                endif
                If (x(i) + x(k) .eq. 0.) Then
                   d(i, k) = d(i, k) + 1
                endif
            enddo
        enddo
      enddo
C
C  Matriz de Baroni
C
      if (tmatriz.eq.1) then
            do i = 1,ncol
               mprox(i, i) = 1
                do j = i + 1, ncol
                   cc = C(i, j)
                   dd = d(i, j)
                   RAD = cc * dd
                   BAR_IJ = ((Sqrt(RAD) + C(i, j)) /
     c               (Sqrt(RAD) + AB(i) + AB(j) - C(i, j)))
                   mprox(i, j) = BAR_IJ
                   mprox(j, i) = BAR_IJ
                enddo
             enddo
C
C  Matriz de Jaccard
C
      else
            do i = 1,ncol
               mprox(i, i) = 1
                do j = i + 1, ncol
                    BAR_IJ = C(i, j) / (AB(i) + AB(j) - C(i, j))
                    mprox(i, j) = BAR_IJ
                    mprox(j, i) = BAR_IJ
                enddo
             enddo
      end if
      
      end


C
C   Pasa de una matriz de Baroni(n,n) a una de distancias mprox((n*n-1)/2)
C   mprox corresponde a la parte inferior de una matriz cuadrada nxn.
C   N¢tese que no se incluye la diagonal principal
C tmat: Tipo Matriz
C
      SUBROUTINE mprox2mclus (mprox, mclus, n, tmat, ifail)
      integer n, tmat, ifail
      integer  i, j, k
      double precision mclus(n*(n-1)/2), mprox(n, n)
C
C Matriz de Baroni
C
      ifail=-1
      if (tmat.eq.1) then
         k=0
         do i = 2,n
            do j = 1, i-1
                k=k+1
                mclus(k) = 1.d0 - mprox(i,j)
            enddo
         enddo
         ifail=0
      end if
C
C Matrices de Correlaci¢n
C
C   Para realizar el an lisis cluster, necesitamos una matriz de distancias,
C   todos los valores deben ser positivos.
C tmat=3 => matriz de correlaciones (pearson, kendall, spearman)
      if (tmat.eq.3) then
         k=0
         do i = 2, n
            do j = 1, i-1
                k=k+1
                mclus(k) = 1.d0 + mprox(i,j)
            enddo
         enddo
         ifail=0
      end if

      END

C -----------------------------------------------------------------------------
C
C   RUTINAS PARA REALIZAR EL ANALISIS CLUSTER
C

      SUBROUTINE macocluster(METHOD,N,D,ILC,IUC,CD,IORD,DORD,IWK,IFAIL)
	!dec$attributes alias :: G03ECF
	!DEC$ATTRIBUTES dllexport :: G03ECF

C     MARK 16 RELEASE. NAG COPYRIGHT 1993.
C
C     Performs Hierarchical Cluster Analysis using distance matrix D.
C
C     METHOD indicates which type is performed
C     METHOD = 1 - single link
C     METHOD = 2 - complete link
C     METHOD = 3 - group average
C     METHOD = 4 - centroid
C     METHOD = 5 - median
C     METHOD = 6 - minimum variance
C
C     ILC, IUC and CD give information about the clustering process
C
C     IORD and DORD give information for printing the dendrogram
C
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='G03ECF')
C     .. Scalar Arguments ..
      INTEGER           IFAIL, METHOD, N
C     .. Array Arguments ..
      DOUBLE PRECISION  CD(N-1), D(N*(N-1)/2), DORD(N)
      INTEGER           ILC(N-1), IORD(N), IUC(N-1), IWK(2*N)
C     .. Local Scalars ..
      DOUBLE PRECISION  DMIN
      INTEGER           I, IERROR, IFAULT, ITH, JTH, NN
C     .. Local Arrays ..
CCC      CHARACTER*80      REC(1)
C     .. External Functions ..
      DOUBLE PRECISION  G03ECW
C     .. External Subroutines ..
      EXTERNAL          G03ECW, G03ECX, G03ECY, G03ECZ
C     .. Executable Statements ..
      IERROR = 0
      IF (METHOD.LT.1 .OR. METHOD.GT.6) THEN
         IERROR = 1
C         WRITE (REC,FMT=99999) METHOD
      ELSE IF (N.LT.2) THEN
         IERROR = 1
C         WRITE (REC,FMT=99998) N
      ELSE
         NN = N*(N-1)/2
         DO 20 I = 1, NN
            IF (D(I).LT.0.0D0) THEN
               IERROR = 2
            END IF
   20    CONTINUE
C         IF (IERROR.EQ.2) THEN
C            WRITE (REC,FMT=99997)
C         END IF
      END IF
      IF (IERROR.EQ.0) THEN
         DO 40 I = 1, N
            IWK(I) = I
            IWK(N+I) = 1
   40    CONTINUE
         DO 60 I = 1, N - 1
            CALL G03ECX(N,D,IWK,ITH,JTH,DMIN)
            IF (I.NE.N-1) THEN
        CALL G03ECY(METHOD,G03ECW,N,D,IWK,IWK(N+1),ITH,JTH,DMIN)
            END IF
            ILC(I) = JTH
            IUC(I) = ITH
            CD(I) = DMIN
            IF (I.GT.1) THEN
               IF (DMIN.LT.CD(I-1)) THEN
                  IERROR = 3
C                  WRITE (REC,FMT=99996)
               END IF
            END IF
   60    CONTINUE
         CALL G03ECZ(N,ILC,IUC,CD,IORD,DORD,IWK,IFAULT)
         IF (IFAULT.NE.0) THEN
            IERROR = 4
         END IF
      END IF
      CONTINUE
      IFAIL = IERROR
C
      RETURN
C
C99999 FORMAT (1X,'** On entry, METHOD.lt.1 .or. METHOD.gt.6: METHOD = ',
C     *       I16)
C99998 FORMAT (1X,'** On entry, N.lt.2: N = ',I16)
C99997 FORMAT (1X,'** On entry, at least one element of D is negative.')
C99996 FORMAT (1X,'** Minimum cluster distance not increasing, dendrogr',
C     *       'am invalid.')
      END


      SUBROUTINE G03ECX(N,D,INC,ITH,JTH,DMIN)
	!dec$attributes alias :: G03ECX
	!DEC$ATTRIBUTES dllexport :: G03ECX


C     MARK 16 RELEASE. NAG COPYRIGHT 1993.
C
C     Selects the two clusters to merge
C
C     Only units with IND gt 0 are considered
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  DMIN
      INTEGER           ITH, JTH, N
C     .. Array Arguments ..
      DOUBLE PRECISION  D(N*(N-1)/2)
      INTEGER           INC(N)
C     .. Local Scalars ..
      INTEGER           I, J, K, L
C     .. Executable Statements ..
C
      DO 20 I = 2, N
         IF (INC(I).GT.0) THEN
            L = I
            GO TO 40
         END IF
   20 CONTINUE
   40 CONTINUE
      K = (L-1)*(L-2)/2 + 1
      DMIN = D(K)
      ITH = L
      JTH = 1
      DO 80 I = L, N
         IF (INC(I).GT.0) THEN
            DO 60 J = 1, I - 1
               IF (INC(J).GT.0) THEN
                  IF (D(K).LE.DMIN) THEN
                     DMIN = D(K)
                     ITH = I
                     JTH = J
                  END IF
               END IF
               K = K + 1
   60       CONTINUE
         ELSE
            K = K + I - 1
         END IF
   80 CONTINUE
      RETURN
      END

      DOUBLE PRECISION FUNCTION G03ECW(ITYPE,DKI,DKJ,DIJ,NI,NJ,NK)
	!dec$attributes alias :: G03ECW
	!DEC$ATTRIBUTES dllexport :: G03ECW

C     MARK 16 RELEASE. NAG COPYRIGHT 1993.
C
C     Computes the distance (DKI) from cluster k to cluster
C     formed by merging clusters i and j.
C     ITYPE indicates method:
C           1 - slingle link
C           2 - complete link
C           3 - group average
C           4 - centroid
C           5 - median
C           6 - minimum variance
C
C     Note the values of NI, NJ and NK are asummed gt 0
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION                 DIJ, DKI, DKJ
      INTEGER                          ITYPE, NI, NJ, NK
C     .. Intrinsic Functions ..
      INTRINSIC                        DBLE, MAX, MIN
C     .. Executable Statements ..
C
      IF (ITYPE.EQ.1) THEN
         G03ECW = MIN(DKI,DKJ)
      ELSE IF (ITYPE.EQ.2) THEN
         G03ECW = MAX(DKI,DKJ)
      ELSE IF (ITYPE.EQ.3) THEN
         G03ECW = (DBLE(NI)*DKI+DBLE(NJ)*DKJ)/DBLE(NI+NJ)
      ELSE IF (ITYPE.EQ.4) THEN
         G03ECW = (DBLE(NI)*DKI+DBLE(NJ)*DKJ-DBLE(NI)*DBLE(NJ)
     *            *DIJ/DBLE(NI+NJ))/DBLE(NI+NJ)
      ELSE IF (ITYPE.EQ.5) THEN
         G03ECW = 0.5D0*DKI + 0.5D0*DKJ - 0.25D0*DIJ
      ELSE IF (ITYPE.EQ.6) THEN
         G03ECW = (DBLE(NI+NK)*DKI+DBLE(NJ+NK)*DKJ-DBLE(NK)*DIJ)
     *            /DBLE(NI+NJ+NK)
      END IF
      RETURN
      END





      SUBROUTINE G03ECZ(N,ILC,IUC,CD,IORD,DORD,IND,IERROR)
	!dec$attributes alias :: G03ECZ
	!DEC$ATTRIBUTES dllexport :: G03ECZ
C     MARK 16 RELEASE. NAG COPYRIGHT 1993.
C
C     Computes information for producing dendrogram
C
C     ILC(i) contains the cluster that joins IUC(i) at the
C     ith join. ILC(i) gt IUC(i)
C
C     IORD is the order for the dendrogram and object IORD(i)
C     joins cluster with object IORD(i-1) at distance DORD(i)
C
C     IND contains information on which part of the ILC is to be
C     searched
C
C     IERROR returns 1 if algorithm fails
C
C     .. Scalar Arguments ..
      INTEGER           IERROR, N
C     .. Array Arguments ..
      DOUBLE PRECISION  CD(N-1), DORD(N)
      INTEGER           ILC(N-1), IND(N), IORD(N), IUC(N-1)
C     .. Local Scalars ..
      INTEGER           I, ILINK, J, L
C     .. Executable Statements ..
C
      IERROR = 0
      DO 20 I = 1, N
         IND(I) = 1
   20 CONTINUE
      L = 1
      ILINK = 1
      IORD(1) = 1
      DORD(N) = CD(N-1)
      DO 140 I = 2, N
   40    CONTINUE
         DO 60 J = IND(L), N - 1
            IF (ILC(J).EQ.ILINK) THEN
               GO TO 100
            END IF
   60    CONTINUE
         IF (L.EQ.1) THEN
            IERROR = 1
            GO TO 160
         END IF
         IND(L) = N
   80    L = L - 1
         IF (IND(L).LT.N) THEN
            ILINK = IORD(L)
            GO TO 40
         ELSE IF (L.GT.1) THEN
            GO TO 80
         ELSE
            IERROR = 1
            GO TO 160
         END IF
  100    CONTINUE
         IND(L) = J + 1
         ILINK = IUC(J)
         IORD(I) = ILINK
         DORD(I-1) = CD(J)
  120    L = L + 1
         IF (L.GT.N) THEN
            IERROR = 1
            GO TO 160
         ELSE IF (IND(L).EQ.N) THEN
            GO TO 120
         END IF
  140 CONTINUE
  160 CONTINUE
      RETURN
	END

*
* Transformación de Yates
*
	SUBROUTINE TYATES(A, B, C, D)
	!dec$attributes alias     :: TYATES
	!DEC$ATTRIBUTES dllexport :: TYATES


	DOUBLE PRECISION A, B, C, D

	IF (A*B .eq. C*D) goto 100

	IF (A*B .lt. C*D) THEN
		A = A + 0.5d0
		B = B + 0.5d0
		C = C - 0.5d0
		D = D - 0.5d0
	END IF

	IF (A*B.gt. C*D) THEN
		A = A - 0.5d0
		B = B - 0.5d0
		C = C + 0.5d0
		D = D + 0.5d0
	END IF

100	RETURN
	END


	SUBROUTINE CFRONTERA(A, B, C, D, G)
	!dec$attributes alias :: CFRONTERA
	!DEC$ATTRIBUTES dllexport :: CFRONTERA

	DOUBLE PRECISION A, B, C, D, G
	DOUBLE PRECISION C1, C2, C3


	CALL TYATES(A, B, C, D)

	C1 = A*LOG(A) + B*LOG(B) + C*LOG(C) + D*LOG(D)
	C2 = (A+C)*LOG(A+C)+(D+B)*LOG(D+B)+(A+D)*LOG(A+D)+(C+B)*LOG(C+B)
	C3 = (A+B+C+D)*LOG(A+B+C+D)

	G= 2*(C1-C2+C3)

	RETURN
	END


      SUBROUTINE G03ECY(ITYPE,G03ECW,N,D,INC,NC,JOIN1,JOIN2,DMIN)
	!dec$attributes alias :: G03ECY
	!DEC$ATTRIBUTES dllexport :: G03ECY
C     MARK 16 RELEASE. NAG COPYRIGHT 1993.
C
C     Updates distance matrix when clusters JOIN1 and JOIN2 are
C     merged. Function G03ECW computes new distances
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  DMIN
      INTEGER           ITYPE, JOIN1, JOIN2, N
C     .. Array Arguments ..
      DOUBLE PRECISION  D(N*(N-1)/2)
      INTEGER           INC(N), NC(N)
C     .. Function Arguments ..
      DOUBLE PRECISION  G03ECW
      EXTERNAL          G03ECW
C     .. Local Scalars ..
      DOUBLE PRECISION  DKL, DKU
      INTEGER           I, JL, JU, KL, KU, NL, NU
C     .. Executable Statements ..
C
      IF (JOIN1.LT.JOIN2) THEN
         JL = JOIN1
         JU = JOIN2
      ELSE
         JL = JOIN2
         JU = JOIN1
      END IF
      INC(JU) = 0
      NL = NC(JL)
      NU = NC(JU)
      NC(JL) = NL + NU
      NC(JU) = 0
      KL = (JL-1)*(JL-2)/2
      KU = (JU-1)*(JU-2)/2
      DO 20 I = 1, JL - 1
         KL = KL + 1
         KU = KU + 1
         DKL = D(KL)
         DKU = D(KU)
         IF (INC(I).GT.0) D(KL) = G03ECW(ITYPE,DKL,DKU,DMIN,NL,NU,NC(I))
   20 CONTINUE
C
C     Skip JL
C
      KL = KL + 1
      KU = KU + 1
C
      DO 40 I = JL + 1, JU - 1
         KL = KL + I - 2
         KU = KU + 1
         DKL = D(KL)
         DKU = D(KU)
         IF (INC(I).GT.0) D(KL) = G03ECW(ITYPE,DKL,DKU,DMIN,NL,NU,NC(I))
   40 CONTINUE
C
C     Skip JU
C
      KL = KL + JU - 2
      KU = KU + 1
C
      DO 60 I = JU + 1, N
         KL = KL + I - 2
         KU = KU + I - 2
         DKL = D(KL)
         DKU = D(KU)
         IF (INC(I).GT.0) D(KL) = G03ECW(ITYPE,DKL,DKU,DMIN,NL,NU,NC(I))
   60 CONTINUE
      RETURN
      END
C
C------------------------
C------------------------
      SUBROUTINE CalculaMSignos2(n, vmax, vmin, msignos, mprox,
     c                          tipodatos,ifail)

C 2: Vamos a introducir el c lculo de la msignos para Jaccard
C

C INPUT
C mprox es la matriz de Baroni
C
      INTEGER N, tipodatos
      double precision vmax, vmin, mprox(n, n)

C OUTPUT
      INTEGER ifail, MSIGNOS (N, N)

C Auxiliares-Locales

      double precision valor
      INTEGER naux, signo
C -------------
C INICIO
      ifail=0
      naux = n

C     El n£mero de caracteres no puede ser < 1
      If (naux .lt. 1) IFAIL = 1

C Case 1, 2, 3  ' Matriz de Baroni
C      IF (TipoDatos .LT. 4) THEN
      If ((TipoDatos .gt. 3) .or. (TipoDatos.lt.1)) IFAIL = 2

      IF (IFAIL.NE.0) GOTO 1000
      DO  i = 1,N - 1
          DO j = i + 1, N
              signo = 0
              valor = MProx(i, j)
              If (vmin .GT. valor) signo = -1
              If (vmax .LT. valor) signo = 1

              mSignos(i, j) = signo
              mSignos(j, i) = signo
          ENDDO
      ENDDO
C Case 4     ' JACCARD  condiciones IRREVERSIBLES
C      ELSE

C      DO  i = 1,NVar - 1
C          DO j = i + 1, NVar
C            A = IMax(AB(i), AB(j)): B = IMin(AB(i), AB(j))
C            signo = " 0 "
C            valor = MatrizProx(i, j)
C            signo = SignoJaccard(A, B, valor)
C
C            matrizSignos(i, j) = signo
C            matrizSignos(j, i) = signo
C        Next j
C    Next i

C Case 5     ' JACCARD  condiciones REVERSIBLES

C      END IF
1000  CONTINUE
      END

C
C --------------------------------------------------------------------------------
C
      SUBROUTINE CalculaMSignos(n, vmax, vmin, msignos, mprox,
     c                          tipodatos,ifail)

C INPUT
C mprox es la matriz de Baroni
C
      INTEGER N, tipodatos
      double precision vmax, vmin, mprox(n, n)

C OUTPUT
      INTEGER ifail, MSIGNOS (N, N)

C Auxiliares-Locales

      double precision valor
      INTEGER naux, signo
C -------------
C INICIO
      ifail=0
      naux = n

C     El n£mero de caracteres no puede ser < 1
      If (naux .lt. 1) IFAIL = 1

C Case 1, 2, 3  ' Matriz de Baroni
C      IF (TipoDatos .LT. 4) THEN
      If ((TipoDatos .gt. 3) .or. (TipoDatos.lt.1)) IFAIL = 2

      IF (IFAIL.NE.0) GOTO 1000
      DO  i = 1,N - 1
          DO j = i + 1, N
              signo = 0
              valor = MProx(i, j)
              If (vmin .GT. valor) signo = -1
              If (vmax .LT. valor) signo = 1

              mSignos(i, j) = signo
              mSignos(j, i) = signo
          ENDDO
      ENDDO
C Case 4     ' JACCARD  condiciones IRREVERSIBLES
C      ELSE

C      DO  i = 1,NVar - 1
C          DO j = i + 1, NVar
C            A = IMax(AB(i), AB(j)): B = IMin(AB(i), AB(j))
C            signo = " 0 "
C            valor = MatrizProx(i, j)
C            signo = SignoJaccard(A, B, valor)
C
C            matrizSignos(i, j) = signo
C            matrizSignos(j, i) = signo
C        Next j
C    Next i

C Case 5     ' JACCARD  condiciones REVERSIBLES

C      END IF
1000  CONTINUE
      END
      
C
C --------------------------------------------------------------------------------
C

      DOUBLE PRECISION  Function calculad(A, B, x, y)
      integer A, B
      DOUBLE PRECISION X, Y, XX

      If (A .EQ. 0) Then
          If (B .EQ. 1) Then
             calculad = (1.d0 / dSqrt(2.d0))
          Else
             calculad = dfloat(0)
          End If
      Else
          xx = A / x
          calculad = (xx * y )/ (Sqrt(xx * xx + y * y))
      End If
      End Function
C
C --------------------------------------------------------------------------------
C

C'
C' Cálculo de la independencia.
C'
      double precision  Function Independencia(aa, bb, cc, dd)
      double precision aa, bb,cc, dd
      double precision aux1, aux2, aux3, A , B,  C , d


C'
C' Devolver c¢digo de error y explicar el error
C'
      A = aa
      B = bb
      C = cc
      d = dd

      Independencia = -99

C'Transformaci¢n de Yates
      If (A * B .lt. C * d) Then
          A = A + 0.5d0
          B = B + 0.5d0
          C = C - 0.5d0
          d = d - 0.5d0
      End If

      If (A * B .gt. C * d) Then
          A = A - 0.5d0
          B = B - 0.5d0
          C = C + 0.5d0
          d = d + 0.5d0
      End If

CSi a pesar de la transformación queda algún valor menor o igual a 0, no podemos hacerlo.


      If ((A * B * C * d) .GT. 0) Then
C Fin transformación de Yates
C Log : Logaritmo Natural
C
        aux1 = A * Log(A) + C * Log(C) + d * Log(d) + B * Log(B)
        aux2 = (A + C) * Log(A + C) + (B + d) * Log(B + d) +
     c         (A + d) * Log(A + d) + (C + B) * Log(C + B)
        aux3 = (A + B + C + d) * Log(A + B + C + d)

        Independencia = 2.d0 * (aux1 - aux2 + aux3)
      End If

      End
C
C-----------------------------------------------------------------------------
C

      DOUBLE PRECISION Function F1(x)
      
      DOUBLE PRECISION x
         F1 = x / Sqrt(x * x + 1)
      End
C
C ----------------------------------------------------------------------------
C
      SUBROUTINE CalculaEtapasPrevias(nvar, ilc, iuc, etapaprevial,
     c                                etapapreviau)
      INTEGER NVAR
      INTEGER ILC(nvar-1), IUC(nvar-1)
      INTEGER etapaprevial(nvar-1), etapapreviau(nvar-1)
C
C Auxiliares
      Integer I,J

      
      etapaprevial(1) = 0
      etapapreviau(1) = 0

      do i = 2, NVAR - 1
        do j = i - 1,1, -1
            If (   (etapaprevial(i) .eq. 0)  .And.
     c          ((ilc(i) .eq. ilc(j)) .Or. (ilc(i) .eq. iuc(j))))
     c                etapaprevial(i) = j

            If ((etapapreviau(i) .eq. 0) .And.
     c           ((iuc(i) .eq. ilc(j)) .Or. (iuc(i) .eq. iuc(j))))
     c                etapapreviau(i) = j

        enddo
      enddo

      End

C
C -------------------------------------------------------
C
      SUBROUTINE ExtremosBaroni(n, vmax, vmin)
      INTEGER N
      DOUBLE PRECISION VMAX, VMIN
C'
C' Seg£n el n£mero de atributos (N) devuelve los valores para los que la
C' similitud (S) es significativa al nivel de confianza del 95%
C'
C' Controlar si ocurre un error
C'
C' n entre 5 y 30 --> Directo
C' n entre 30 y 500 --> Estimaci¢n
C' n mayor de 500 --> Por pantalla
C' Extremos para p=0.05
      INTEGER valores
      Parameter (valores=58)


      DOUBLE PRECISION maximos(5:30), minimos(5:30)
      DOUBLE PRECISION  maximos2(0:valores), minimos2(0:valores)
      integer IND(0:valores) , i
      integer ok
      
C'
C'---------------------------------------------------
C' Valores
C'
      DATA minimos
     c /6*0.0,0.181,0.211,0.232,0.236,0.23,0.25,0.266,0.267,0.27,0.284,
     c  0.285,0.301,0.29,0.3,0.311,0.307,0.32,0.317,0.321,0.328/

      DATA MAXIMOS
     c /0.826,0.645,0.829,0.774,0.789,0.738,0.748,0.739,0.72,0.725,
     c  0.706,0.711,0.706,0.695,0.693,0.684,0.681,0.675,0.672,0.667,
     c  0.664,0.661,0.657,0.659,0.651,0.655/
     
      DATA MINIMOS2
     C /0.328,0.332,0.34,0.347,0.35,0.349,0.359,0.362,0.362,0.365,
     C  0.371,0.371,0.373,0.379,0.379,0.379,0.384,0.385,0.387,0.39,
     C  0.39,0.392,0.395,0.394,.396,0.399,0.398,0.401,0.403,0.402,
     C  0.405,0.406,0.405,0.409,0.408,0.407,0.412,0.413,0.417,0.417,
     C  0.419,0.421,0.421,0.424,0.424,0.427,0.429,0.431,0.433,0.435,
     C  0.436,0.439,0.442,0.444,0.446,0.448,0.452,0.455,0.46/
     
      DATA MAXIMOS2
     C /0.655,0.651,0.648,0.642,0.634,0.631,0.628,0.626,0.624,0.623,
     C  0.621,0.616,0.615,0.614,0.613,0.609,0.607,0.606,0.605,0.604,
     C  0.6,0.6,0.599,0.599,0.595,0.595,0.595,0.594,0.591,0.591,0.591,
     C  0.589,0.588,0.588,0.587,0.585,0.583,0.582,0.58,0.577,0.576,
     C  0.576,0.574,0.572,0.571,0.57,0.568,0.566,0.565,0.563,0.561,
     C  0.558,0.556,0.553,0.552,0.55,0.546,0.543,0.539/
     
      DATA IND
     C /30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,
     C  70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,105,110,115,
     C  120,125,130,135,140,145,150,160,170,180,190,200,220,240,260,
     C  280,300,350,400,500/

C'
C'---------------------------------------------------
C'
      ok=0
      i=1
      If (n .GE. 5 .And. n .LE. 30) Then
        vmax = maximos(n)
        vmin = minimos(n)
      Else If (n .GT. 30 .And. n .LE. 500) Then
        Do While (ok.eq.0 .and. i.le.valores)
           If (IND(i) .eq. n) Then
              vmax = maximos2(i)
              vmin = minimos2(i)
              ok=1
           ElseIf (IND(i) .gt. n) Then
C Revisar cuales son los sentidos de crecimiento/decrecimiento
           vmax=maximos2(i)-((n-IND(i-1))*(maximos2(i)-maximos2(i-1)) /
     c          (IND(i) - IND(i - 1)))
           vmin=minimos2(i-1)+(n-IND(i-1))*((minimos2(i)-minimos2(i-1))/
     c          (IND(i) - IND(i - 1)))
              ok=1
           End If
           i=i+1
        ENDDO
        
      ElseIf (n .GT. 500) Then

      End If

      END

C
C-----------------------------------------------------------------------------
C
C  Probado : RecorreArbol.f
C  Ok: 25-Febrero-2008
C
      SUBROUTINE Poblaciones(nvar, ilc, iuc, etPreviaL, etPreviaU,
     c                       MacoquiListas)
      integer nvar
      integer ilc(nvar-1), iuc(nvar-1)
      integer etPreviaL(nvar-1), etPreviaU(nvar-1)
      integer MacoquiListas(nvar-1, nvar)
C      INTEGER ZonaA((nvar-1, nvar), ZonaB((nvar-1, nvar)
      INTEGER I , J
C
C

C Obtenemos las etapas previas

      CALL CalculaEtapasPrevias(nvar, ilc, iuc, etpreviaL, etPreviaU)


C     MacoquiListas(i,j)  Tenemos una fila para cada nodo:I=1, NVAR-1
C                          Y una columna para cara variable: j=1, NVAR
C
C     MacoquiListas(i,j) = 0 La variable Xi no entra en el nodo j
C                        = -1  La variable Xi ENTRA POR LA ZONA A en el nodo j
C                        =  1  La variable Xi ENTRA POR LA ZONA B en el nodo j
C
      DO I=1, NVAR-1

         IF (etPreviaL(i).eq.0) then
              MacoquiListas(i, ilc(i))=-1
         ELSE
             DO J=1, NVAR
                IF (MacoquiListas(etPreviaL(i), j).ne.0) then
                    MacoquiListas(i, j) = -1
                endif
             ENDDO
         ENDIF

         IF (etPreviaU(i).eq.0) then
              MacoquiListas(i, iuc(i))=1
         ELSE
             DO J=1, NVAR
                IF (MacoquiListas(etPreviaU(i), j).ne.0) then
                    MacoquiListas(i, j) =  1
                endif
             ENDDO
         ENDIF

      ENDDO

      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE CalculaPadresHijos(nvar, etPreviaL, etPreviaU,
     c                              PadresHijos)
      integer nvar
      integer etPreviaL(nvar-1), etPreviaU(nvar-1)
      integer PadresHijos(nvar-1, nvar-1)
      INTEGER I , J
C
C
C      PadresHijos(i,j)  Tenemos una fila y una columna para cada nodo:I=1, NVAR-1
C
C
C Si miramos por filas, tenemos los hijos:
C
C      PadresHijos(i,j)   = 0  El nodo j no es descendiente del nodo i
C                        = -1  El nodo j es descendiente del nodo i por la izquierda
C                        =  1  El nodo j es descendiente del nodo i por la derecha
C
C
CC Tenemor en PadresHijos, los ascendientes mirando por columnas,
CC as¡, si miramos padres tenemos que
CC
CC  PadresHijos (i,j)  = 1 : La etapa i es ascendientes por la derecha de la j
CC                     =-1 : La etapa i es ascendientes por la IZQUIERDA de la j


      DO I=1, NVAR-1

         IF (etPreviaL(i).ne.0) then
             PadresHijos(i, etPreviaL(i))=-1
             DO J=1, NVAR-1
                IF (PadresHijos(etPreviaL(i), j).ne.0) then
                    PadresHijos(i, j) = -1
                endif
             ENDDO
         ENDIF

         IF (etPreviaU(i).ne.0) then
             PadresHijos(i, etPreviaU(i))=1
             DO J=1, NVAR-1
                IF (PadresHijos(etPreviaU(i), j).ne.0) then
                    PadresHijos(i, j) =  1
                endif
             ENDDO
         ENDIF

      ENDDO

      END

C ----------------------------------------------------------------------------
C
      INTEGER Function Significacion(x)
      DOUBLE PRECISION x
C
C' Dado x, supuesto valor calculado de una ji-cuadrado de 1 grado de libertad,
C' devuelve si es significativo o no. Si devuelve
C'  0  ns    : no es significativo: p>0.05
C'  1   *    : 0.05  >= p  > 0.01
C'  2  **    : 0.01  >= p  > 0.001
C'  3 ***    : 0.001 >= p
C'
C' Nota: x, como valor de una ji-cuadrado, debe ser positivo
C
C
      If (x .lt. 3.841) Then
          Significacion = 0
      ElseIf (x .gt. 3.84 .And. x .lt. 6.635) Then
          Significacion = 1
      ElseIf (x .gt. 6.634 .And. x .lt. 10.828) Then
          Significacion = 2
      ElseIf (x .gt. 10.827) Then
          Significacion = 3
      End If

      End Function

C --------------------------------------------------------------------------------
C
      Subroutine Corotipos(NVAR,corotiposSN, vDwa, vDwb,gwa, gwb,
     c                     PadresHijos, ncor)
      INTEGER          NVAR, ncor, corotiposSN(nvar-1,2)
      INTEGER          PadresHijos(nvar-1, nvar-1)
      DOUBLE PRECISION vDwa(nvar-1), vDwb(nvar-1)
      DOUBLE PRECISION gwa(nvar-1), gwb(nvar-1)
      DOUBLE PRECISION auxdwAB, auxgwa, auxgwb

C Funciones
      INTEGER          SIGNIFICACION
C
C' Obtendremos el vector CorotiposSN(1 to 2, 1 to NVar-1),
C' donde para cada i=1,..., NVar-1 toma el valor
C'
C'   0   Si es corotipo
C'   k   Si no es corotipo y k es el nodo donde se incumple por primera
C'       vez la condici¢n de Corotipos.
C'  -1   Valor dw es 0
C'  -2   valor cumple las condiciones previas para ser corotipo pero no es significativo
C'
C'   CorotiposSN(1,%) Se refiere a la Zona A (Parte superior)
C'   CorotiposSN(2,%) Se refiere a la Zona B (Parte inferior)
C'
C' Un nodo i (i=1,...,nvar-1) es corotipo si:
C'
C' Sea j el primer nodo superior a i (j es el padre de i)
C' Ve se la representaci¢n del dendrograma)
C' N¢tese que j no tiene porqu‚ ser i+1
C' Notaremos como m superior a n de la forma m>>n
C'
C'
C'   [1]
C'   Para todo k<<j : Dwa(j) > Dwa(k) y Dwb(k)
C'   Para todo l>>j : Dwa(j) > Dwa(l) si entra por la izquierda (ARRIBA)
C'                           > Dwb(l) si entra por la derecha   (ABAJO)
C'
C'   1-MAY-2002
C'       Si Dwa (j) <= 0 => NO ES COROTIPO
C'       Si Dwa (j)  = 0,7071(TopeDw) =<> ES COROTIPO (lo dejamos que lo obtenga,
C                       por temas de redondeo)
C'       Si   0 < Dwa (j)  < 0,7071(TopeDw)
C'                             => Adem s de cumplir  [1], debe ser significativo
C'
C'
C'
C''''''''''''''''''''''''''''''''''''''''''''''''

      auxdwAB=0.D0
      ncor=0
      DO i = 1, NVar - 1
C----------
C          PARTE IZQUIERDA
C
      auxdwAB = vDwa(i)
      auxgwa =  gwa(i)
      CorotiposSN(i,1) = 0

C Vale 1

      IF  (auxdwAB .GE. 1.D0) THEN
          CorotiposSN(i,1) = 0
C Distinto de 0
      ELSEIF (auxdwAB .le. 0.D-8) then
          CorotiposSN(i,1) = -1
      ELSE
C----------
C NODOS INFERIORES: Miramos Filas de PadresHijos
C
C Mayor que todos los Dwa y Dwb descendientes
         J=0
         DO WHILE ((J.LE.NVAR) .AND. (CorotiposSN(i,1).eq.0))
            J=J+1
C            IF ((PadresHijos(i, j) .eq. 1) .AND.
C     c             ((vDwa(j).ge.auxdwAB) .or. (vDwb(j).ge.auxdwAB)) )
C     c                   CorotiposSN(i,1)=j

            IF ((PadresHijos(i, j) .eq. -1) .AND.
     c             ((vDwa(j).ge.auxdwAB) .or. (vDwb(j).ge.auxdwAB)) )
     c                   CorotiposSN(i,1)=j

         ENDDO
C----------
C NODOS SUPERIORES: Miramos columnas de PadresHijos
C
C Mayor que todos los Dwa(-1) / Dwb (1) ascendientes
         J=0
         DO WHILE ((J.LE.NVAR) .AND. (CorotiposSN(i,1).eq.0))
            J=J+1
            IF ((PadresHijos(j, i) .eq.-1) .AND. (vDwa(j).ge.auxdwAB))
     c                   CorotiposSN(i,1)=j
     
            IF ((PadresHijos(j, i) .eq.1) .AND. (vDwb(j).ge.auxdwAB))
     c                   CorotiposSN(i,1)=j
         ENDDO

C Singnificativo   el correspondiente gwa
         IF ((CorotiposSN(i,1).eq.0) .AND. (Significacion(auxgwa).eq.0))
     c       CorotiposSN(i,1)=-2
      end if

C Contamos corotipos
         IF (CorotiposSN(i,1).eq.0) ncor = ncor+1
      
C----------
C          PARTE DERECHA

      auxdwAB = vDwB(i)
      auxgwb =  gwb(i)
      CorotiposSN(i,2) = 0

      IF  (auxdwAB .GE. 1.D0) THEN
          CorotiposSN(i,2) = 0
C Distinto de 0
      ELSEIF (auxdwAB .le. 0.D-8) then
          CorotiposSN(i,2) = -1
      ELSE
C----------
C NODOS INFERIORES: Miramos Filas de PadresHijos
C
C Mayor que todos los Dwa y Dwb descendientes
         J=0
         DO WHILE ((J.LE.NVAR) .AND. (CorotiposSN(i,2).eq.0))
            J=J+1
            IF ((PadresHijos(i, j) .eq. 1) .AND.
     c             ((vDwa(j).ge.auxdwAB) .or. (vDwb(j).ge.auxdwAB)) )
     c                   CorotiposSN(i,2)=j
         ENDDO
C----------
C NODOS SUPERIORES: Miramos columnas de PadresHijos
C
C Mayor que todos los Dwa(-1) / Dwb (1) ascendientes
         J=0
         DO WHILE ((J.LE.NVAR) .AND. (CorotiposSN(i,2).eq.0))
            J=J+1
            IF ((PadresHijos(j, i) .eq.-1) .AND. (vDwa(j).ge.auxdwAB))
     c                   CorotiposSN(i,2)=j

            IF ((PadresHijos(j, i) .eq.1) .AND. (vDwb(j).ge.auxdwAB))
     c                   CorotiposSN(i,2)=j
         ENDDO

C Singnificativo   el correspondiente gwa
         IF ((CorotiposSN(i,2).eq.0) .AND. (Significacion(auxgwb).eq.0))
     c       CorotiposSN(i,2)=-2
     
      end if

C Contamos corotipos
      IF (CorotiposSN(i,2).eq.0) ncor = ncor+1

      ENDDO

      END

CCCCCCCCCCCCCCCCCCC

      SUBROUTINE  corotiposelementos (nvar, MacoquiListas, CorotiposSN,
     c                                corotipos_ele)
      Integer nvar, MacoquiListas(NVAR-1, NVAR)
      Integer CorotiposSN(2, nvar-1), corotipos_ele(nvar)
      integer ncor

C Corotipos propiamente dichos
      ncor=0
C Parte Izquierda (-1)
      do i= 1, nvar-1
         if (corotiposSN(1,i).eq.0) then
            ncor=ncor+1
            do j=1, nvar
                if (MacoquiListas(i,j).eq.-1) corotipos_ele(j) = ncor
            enddo
         end if
         
C Parte Derecha (1)
         if (corotiposSN(2,i).eq.0) then
            ncor=ncor+1
            do j=1, nvar
                if (MacoquiListas(i,j).eq.1) corotipos_ele(j) = ncor
            enddo
         end if
         
      enddo
C Patrones substituci¢n gradual
C 1-dic-2010: S¢lo se trabajar n con los corotipos propiamente dichos
C      do i= 1, nvar-1
C         if (corotiposSN(1,i).eq.-2) then
C            ncor=ncor+1
C            do j=1, nvar
C                if (MacoquiListas(i,j).eq.-1) corotipos_ele(j) = ncor
C            enddo
C         end if

C Parte Derecha (1)
C         if (corotiposSN(2,i).eq.-2) then
C            ncor=ncor+1
C            do j=1, nvar
C                if (MacoquiListas(i,j).eq.1) corotipos_ele(j) = ncor
C            enddo
C         end if

C      enddo

      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  MDCor
C' Matriz de distancias entre los elementos y los corotipos de dimensión
C' N+1 x NCorotipos, donde  la distancia de cada elemento a un corotipo es la
C' media de las distancias de ese elemento a cada uno de los elementos del
C' corotipo.
C' La fila N+1, se construye con la suma de las distancias de cada columna.

      SUBROUTINE cMDCor (n, ncor, cor_ele, mprox, mdcor)
      integer n
      integer cor_ele(n)
      double precision mprox(n,n), mdcor(n+1, ncor), daux(ncor)
      integer nele(ncor)
C
      do j=1, ncor
         daux(j) = 0.d0
         nele(j) = 0
         mdcor(n+1, j) = 0.d0
      end do

      do i=1, n
         do j = 1, n
            if(cor_ele(j).ne.0) then
                nele(cor_ele(j)) =   nele(cor_ele(j)) + 1
                daux(cor_ele(j)) =   daux(cor_ele(j)) +  mprox(i,j)
            endif
         enddo

         do j=1, ncor
            mdcor(i,j) = daux(j) / nele(j)
            daux(j) = 0.d0
            nele(j) = 0
            mdcor(n+1, j) = mdcor(n+1, j) + mdcor(i,j)
         end do
      enddo

      END


CCCCCCCCCCCCCCCCCCCC

      SUBROUTINE TodosValoresMacoqui(nvar, msignos, MacoquiListas,
     C                               MacoquiDatos, MacoquiResultados)
     
      Integer nvar, msignos(nvar, nvar), MacoquiListas(NVAR-1, NVAR)
C      INTEGER MacoquiListas(NVAR-1, NVAR)
      
      Double Precision MacoquiDatos(NVar-1, 14),
     C                 MacoquiResultados(Nvar-1,12)
C
C MacoquiListas contiene para cada nodo, las localidades de cada zona,
C   MacoquiListas(%, 1) Lista con las localidades de la Zona A
C   MacoquiListas(%, 2) Lista con las localidades de la Zona B
C
C MacoquiDatos es una matriz que contiene los valores iniciales para
C obtener las fronteras/regiones.
C Definici¢n:
C   Cada fila corresponde a cada uno de los nodos que se obtienen en el
C   análisis cluster y cada columna a:
C       0.- Similitud entre los nodos unidos en la etapa i
C       1.- N£mero de localidades en la zona A
C       2.- N£mero de localidades en la zona B
C       3.- N£mero de + en la Zona A
C       4.- N£mero de localidades con alg£n + en la Zona A
C       5.- N£mero de - en la Zona A
C       6.- N£mero de localidades con alg£n - en la Zona A
C  7 - 10.- Idem para la Zona B
C 11 - 14.- Idem para la Zona AxB
C

CCuando hacemos Click sobre alguno de los nodos
C queremos que obtenga los datos correspondientes a dichas
C localidades.
CAbre formulario MACOQUI y lo rellena con los datos correspondientes.
CTambi‚n tendr  que obtener la matriz de signos asociada
C
C MacoquiResultados contiene para cada fila (para cada nodo obtenido en el Cluster)
C los valores resultantes se ejecutar Macoqui, o sea, en cada colunna tenemos:
C
C   1.- dw          2.- dwa         3.- dwb
C   4.- ds          5.- dsa         6.- dsb
C   7.- Gw          8.- Gwa         9.- Gwb
C  10.- Gs         11.- Gsa        12.- Gsb
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C Var Auxiliares
      DOUBLE PRECISION AuxResMacoqui(12)
      INTEGER  ListaA(nvar), ListaB(nvar)
      INTEGER la, lb, na, nb, nab, pa, pb, pab
      INTEGER a1, b1, ab1, a2, b2, ab2
      INTEGER i, NodoAnalizado
C
C  Proceso

      DO  NodoAnalizado = 1, NVar - 1
C          EtapaL = 0
C          EtapaU = 0

C Formaci¢n de los grupos a partir del nodo seleccionado.
C Los grupos ser n:
C     ListaA: parte rama superior/izquierda del nodo
C     De un nodo (una fila de MacoquiListas), los -1
C
C     ListaB: parte rama la inferior/derecha del nodo
C     De un nodo (una fila de MacoquiListas), los 1
C
      la=0
      lb=0
      DO i=1, NVAR
         ListaA(i)= 0
         ListaB(i)= 0
         IF (MacoquiListas(NodoAnalizado,i).eq.-1) then
            la=la+1
            ListaA(la) = i
         ELSEIF (MacoquiListas(NodoAnalizado,i).eq.1) THEN
            lb=lb+1
            ListaB(lb) = i
         ENDIF
      ENDDO

C Obtenci¢n de los par metros para Macoqui-->MacoquiDatos

      CALL ParametrosMacoqui(listaA, la, listaB, lb,
     c           msignos, NVAR,
     c           pa, pb, pab, na, nb, nab,
     c           a1, b1, ab1, a2, b2, ab2)
     
C Dada la matriz de signos obtenida (matrizSignos), y dos listas de
C localidades (ListaA, ListaB), obtendremos los siquientes parámetros
C
C
C                                   A           B           AxB
C   Número de localidades         1.la        2.lb
C   Localidades con algún +       3.pa        4.pb        5.pab
C   Localidades con algún -       6.na        7.nb        8.nab
C   Número de +                   9.a1       10.b1       11.ab1
C   Número de -                  12.a2       13.b2       14.ab2

C ===============================================
      MacoquiDAtos(NodoAnalizado, 1) =  la
      MacoquiDAtos(NodoAnalizado, 2) =  lb
      MacoquiDAtos(NodoAnalizado, 3) =  pa
      MacoquiDAtos(NodoAnalizado, 4) =  pb
      MacoquiDAtos(NodoAnalizado, 5) =  pab
      MacoquiDAtos(NodoAnalizado, 6) =  na
      MacoquiDAtos(NodoAnalizado, 7) =  nb
      MacoquiDAtos(NodoAnalizado, 8) =  nab
      MacoquiDAtos(NodoAnalizado, 9) =  a1
      MacoquiDAtos(NodoAnalizado,10) =  b1
      MacoquiDAtos(NodoAnalizado,11) =  ab1
      MacoquiDAtos(NodoAnalizado,12) =  a2
      MacoquiDAtos(NodoAnalizado,13) =  b2
      MacoquiDAtos(NodoAnalizado,14) =  ab2

C
C FIN Obtenci¢n de los par metros para Macoqui-->MacoquiDatos
C

      CALL  DatosMacoqui(AuxResMacoqui, la, lb,
     c           pa , pb , pab, na , nb , nab ,
     c           a1 , b1 , ab1, a2 , b2 , ab2)

      do i = 1,12
        MacoquiResultados(NodoAnalizado, i) = AuxResMacoqui(i)
      enddo

      ENDDO

      END
C
C-----------------------------------------------------------------------------
C
      SUBROUTINE ParametrosMacoqui(listaA, la, listaB, lb,
     c           matrizsignos, NVAR,
     c           pa , pb , pab, na , nb , nab ,
     c           a1 , b1 , ab1, a2 , b2 , ab2)

      INTEGER MATRIZSIGNOS(nvar, nvar)
      integer na, nb, nab, pa, pb, pab
      integer a1, b1, ab1, a2, b2, ab2
      integer la, lb
      INTEGER listaA(la), listaB(lb)
C      INTEGER listaA(nvar), listaB(nvar)
C
C Auxiliares
      INTEGER i, j, fila, col
      INTEGER valorespos(la+lb), valoresneg(la+lb)

C'
C' Dada la matriz de signos obtenida (matrizSignos), y dos listas de
C' localidades (ListaA, ListaB), obtendremos los siquientes par metros
C'
C'MacoquiDatos:
C'                                   A           B           AxB
C'   N£mero de localidades           la          lb
C'   Localidades con alg£n +         pa          pb          pab
C'   Localidades con alg£n -         na          nb          nab
C'   N£mero de +                     a1          b1          ab1
C'   N£mero de -                     a2          b2          ab2
C'

      do i=1, la+lb
         valorespos(i)=0
         valoresneg(i)=0
      enddo
C' Valores referidos a la lista A
      a1=0
      a2=0
      do i = 1, la
         do j = i + 1, la
            fila = listaA(i)
            col  = listaA(j)
            If (matrizSignos(fila, col).eq.1) Then
                a1 = a1 + 1
                valorespos(i) = 1
                valorespos(j) = 1
            End If

            If (matrizSignos(fila, col).eq.-1) Then
                a2 = a2 + 1
                valoresneg(i) = 1
                valoresneg(j) = 1
            End If
         enddo
      enddo
      
      pa=0
      na=0
      do i = 1, la
        pa= pa + valorespos(i)
        na= na + valoresneg(i)
        valorespos(i)=0
        valoresneg(i)=0
      enddo

C
C Valores referidos a la lista B
C
      b1=0
      b2=0
      do i = 1, lb
         do j = i + 1, lb
            fila = listaB(i)
            col  = listaB(j)
            If (matrizSignos(fila, col).eq.1) Then
                b1 = b1 + 1
                valorespos(i) = 1
                valorespos(j) = 1
            End If
            If (matrizSignos(fila, col).eq.-1) Then
                b2 = b2 + 1
                valoresneg(i) = 1
                valoresneg(j) = 1
            End If
         enddo
      enddo
      
      pb=0
      nb=0
      do i = 1, lb
        pb= pb + valorespos(i)
        nb= nb + valoresneg(i)
        valorespos(i)=0
        valoresneg(i)=0
      enddo

C
C Valores referidos a la lista AB
C
      ab1=0
      ab2=0
      do i = 1, la
         do j = 1,lb
            fila = listaa(i)
            col  = listab(j)
            If (matrizSignos(fila, col).eq.1) Then
                ab1 = ab1 + 1
                valorespos(i) = 1
                valorespos(la+j) = 1
            End If
            If (matrizSignos(fila, col).eq.-1) Then
                ab2 = ab2 + 1
                valoresneg(i) = 1
                valoresneg(la+j) = 1
            End If
         enddo
      enddo
      
      pab=0
      nab=0
      do i=1, la+lb
         pab=pab+valorespos(i)
         nab=nab+valoresneg(i)
         valorespos(i)=0
         valoresneg(i)=0
      enddo

      End


C ==========================================================================================================
C
C    Fecha: 2-Marzo-2006
C
      SUBROUTINE  DatosMacoqui(ResMacoqui, la, lb,
     c           pa , pb , pab, na , nb , nab ,
     c           a1 , b1 , ab1, a2 , b2 , ab2)

      integer la, lb
      integer na, nb, nab, pa, pb, pab
      integer a1, b1, ab1, a2, b2, ab2
      DOUBLE PRECISION ResMacoqui(12)

C'
C'Variables locales
C'
      DOUBLE PRECISION TopeDw
      Parameter (TopeDw = 0.707106781186548)
      INTEGER lab


      DOUBLE PRECISION ta, tb, t_ab,  yb1,  ya1
      DOUBLE PRECISION ya2, yb2,  yab2, yab1
      DOUBLE PRECISION da1, db1, da2, db2, d3, d4
      DOUBLE PRECISION dwpa, dwpb, dwa, dwb, dw, dsp, ds, dsa, dsb
      DOUBLE PRECISION af1, af2, bf1, bf2, abf1, abf2, a1b1w, a1w
      DOUBLE PRECISION B1W, A2B2W, A2W, B2W, abw1, abw2, a1b1s, a1s
      DOUBLE PRECISION b1s, a2b2s, a2s, b2s, abs1, abs2, gw, gwa, gwb
      DOUBLE PRECISION gs, gsa, gsb

C Funciones usadas

      DOUBLE PRECISION calculad, F1, independencia

C'Valores iniciales
      ta = la * ((la - 1.d0) / 2.d0)
      tb = lb * ((lb - 1.d0) / 2.d0)

      t_ab = la * lb
      lab = la + lb

C' C lculo de Dw y Ds
      ya1 = dfloat(pa) / dfloat(la)
      yb1 = dfloat(pb) / dfloat(lb)

C' C lculo de da1
      da1 = calculad(a1, la, ta, ya1)

C' C lculo de db1
      db1 = calculad(b1, lb, tb, yb1)

C' Nota no confundir NA y NB con el número de localidades en A y en B
C'
      ya2 = dfloat(na) / dfloat(la)
      yb2 = dfloat(nb) / dfloat(lb)

C' C lculo de da2
      da2 = calculad(a2, 0, ta, ya2)

C' C lculo de db2
      db2 = calculad(b2, 0, tb, yb2)

C'xab2 = ab2 / t_ab
      yab2 = dfloat(nab) / dfloat(lab)

      d3 = calculad(ab2, 0, t_ab, yab2)

C'xab1 = ab1 / t_ab
C      yab1 = dfloat(pab / lab)
      yab1 = dfloat(pab) / dfloat(lab)

      d4 = calculad(ab1, 0, t_ab, yab1)

      dwpa = da1 - da2
      dwpb = db1 - db2

C'
C' Para ayudar a su interpretación, dividimos por Raiz(2)/2 los dw

      Dwa = dwpa - d4
      Dwa = Dwa / TopeDw
      Dwb = dwpb - d4
      Dwb = Dwb / TopeDw
      dw = (Dwa + Dwb) / 2.d0
      dw = dw

      dsp = d3 - d4
      ds = (dsp - (da2 + db2) / 2.d0) / TopeDw
      dsa = (dsp - da2) / TopeDw
      dsb = (dsp - db2) / TopeDw

      af1 = 0
      af2 = 0
      bf1 = 0
      bf2 = 0
      abf1 = 0
      abf2 = 0

      If (a1 .ne. 0) af1 = a1 * (da1 / F1(a1 / ta))

      If (a2 .ne. 0)  af2 = a2 * (da2 / F1(a2 / ta))


      If (b1 .ne. 0)  bf1 = b1 * (db1 / F1(b1 / tb))
      If (b2 .ne. 0)  bf2 = b2 * (db2 / F1(b2 / tb))

      If (ab1 .ne. 0) abf1 = ab1 * (d4 / F1(ab1 / t_ab))
      If (ab2 .ne. 0) abf2 = ab2 * (d3 / F1(ab2 / t_ab))

C'
C' C lculo de la matriz colapsada para la frontera d‚bil
C'
      a1b1w = af1 + bf1
      a1w = af1
      b1w = bf1
      a2b2w = af2 + bf2 + (ta - a1 - a2) + (tb - b1 - b2)
      a2w = af2 + (ta - a1 - a2)
      b2w = bf2 + (tb - b1 - b2)
      abw1 = abf1
      abw2 = ab2 + (t_ab - ab1 - ab2)

C'
C' C lculo de la matriz colapsada para la frontera fuerte
C'
      a1b1s = af1 + bf1 + (ta - a1 - a2) + (tb - b1 - b2)
      a1s = af1 + (ta - a1 - a2)
      b1s = bf1 + (tb - b1 - b2)
      a2b2s = af2 + bf2
      a2s = af2
      b2s = bf2
      abs1 = abf1 + (t_ab - ab1 - ab2)
      abs2 = abf2


C'
C' An lisis de independencia (SOKAL y ROHLF, 1986 pag. 295)
C'
C'An lisis de independencia de la frontera débil.
      Gw = Independencia(a1b1w, abw2, a2b2w, abw1)

C An lisis de independencia de la frontera débil para AxA
      Gwa = Independencia(a1w, abw2, a2w, abw1)

C An lisis de independencia de la frontera débil para BxB
      Gwb = Independencia(b1w, abw2, b2w, abw1)

C An lisis de independencia de la frontera fuerte.
      Gs = Independencia(a1b1s, abs2, a2b2s, abs1)

C An lisis de independencia de la frontera fuerte para AxA
      Gsa = Independencia(a1s, abs2, a2s, abs1)

C An lisis de independencia de la frontera fuerte para BxB
      Gsb = Independencia(b1s, abs2, b2s, abs1)

C'
C'  Resultados
      ResMacoqui(1) = dw
      ResMacoqui(2) = Dwa
      ResMacoqui(3) = Dwb

      ResMacoqui(4) = ds
      ResMacoqui(5) = dsa
      ResMacoqui(6) = dsb

      ResMacoqui(7) = Gw
      ResMacoqui(8) = Gwa
      ResMacoqui(9) = Gwb

      ResMacoqui(10) = Gs
      ResMacoqui(11) = Gsa
      ResMacoqui(12) = Gsb


      End


C
C -------------------------------------------------------------------------
C
C
C     JACCARD

      DOUBLE PRECISION Function PJaccardReversible(n, C)
      INTEGER N, C
C'
C'  P = 1- Suma(x=0, .... , C-1)[(Combinatorio(N,x)* VR2,N-x)/ (VR3, N)]
C'
C'
      DOUBLE PRECISION d1
      DOUBLE PRECISION Combinatorio, VRA_B
      INTEGER x

      d1 = 0.D0
      DO x= 0, C - 1
         d1 = d1 + Combinatorio(n, x) * VRA_B(2.d0, C - 1 - x)
      ENDDO
      
      d1 = d1 * VRA_B(2.d0/3.D0, n - C + 1) * VRA_B(1.D0/3.D0, C-1)


      PJaccardReversible = 1.D0 - d1

      END
C
C -----------------------------
C
      DOUBLE PRECISION FUNCTION VRA_B(A, B)
      DOUBLE PRECISION A, AUX
      INTEGER  B
C'
C' Variaciones con repetición de A elementos
C' tomados de b en b.
C' b>= a
C'
C'
      AUX=A
      DO I =2, B
        AUX=AUX * A
      ENDDO
      
      VRA_B = AUX

      END

C
C -----------------------------
C
      DOUBLE PRECISION FUNCTION Combinatorio(n, M)
      INTEGER N, M
      DOUBLE PRECISION AUX
C' Número combinatorio de N sobre M
C'
C'  M > = N
C'
      INTEGER i

      AUX = 1
      DO I= 1, M
         AUX= AUX * (dfloat((n + 1 - i)) / dfloat((M - i + 1)))
      ENDDO
      
      Combinatorio=aux

      END
C
C -------------------------------------------------------------------------
C
      INTEGER Function SignoJaccard(A, B, d)
      INTEGER A, B, auxA, auxB
      DOUBLE PRECISION D
      
C'  Subrutina que compara d con las probabilidades, y rellena el
C'  valor de signo de la siguiente forma:
C'
C'     0       P3(-)     P2(-)     P1(-)     P1(+)     P2(+)    P3(+)        1
C'     |-- ... --|---------|---------|---------|---------|---------|--- ... --|
C'
C' Signo=  ---       --         -          0        +         ++        +++
C'

C'
C' Signo= * en caso de error
C'
      DOUBLE PRECISION p1mas, p2mas, p3mas
      DOUBLE PRECISION p1menos, p2menos, p3menos
      integer condiciones
      

      auxA=A
      auxB=B
      condiciones=1
      Call ValCIndJaccard(auxA, auxB, p1mas, p2mas, p3mas,
     c                    p1menos, p2menos, p3menos, condiciones)

      SignoJaccard = 9

      If (d .LT. p3menos) Then
             SignoJaccard = -3
      ElseIf (d .LT. p2menos) Then
             SignoJaccard = -2
      ElseIf (d .LT. p1menos) Then
             SignoJaccard = -1
      ElseIf (d.LE.p1mas .Or. (d .GE. p1menos .And. p1mas.EQ.-1)) Then
             SignoJaccard = 0
      ElseIf (d .GT. p3mas .And. p3mas .NE. -1) Then
             SignoJaccard = 3
      ElseIf (d .GT. p2mas .And. p2mas .NE. -1) Then
             SignoJaccard = 2
      ElseIf (d .GT. p1mas .And. p1mas .NE. -1) Then
             SignoJaccard = 1
      End If

      End
C
C -------------------------------------------------------------------------
C
      DOUBLE PRECISION  Function PJaccardI(A, B, C)
      INTEGER A, B, C
C'
C' A >= B
C'
      DOUBLE PRECISION dnum, dden
      DOUBLE PRECISION Combinatorio
      INTEGER x

      dnum = 0.D0
      dden = 0.D0
      PJaccardI = 0.D0


      DO x = 0, C - 1
         dnum = dnum + Combinatorio(A + B - x, x)
         dden = dden + Combinatorio(A + B - x, x)
      ENDDO
      
      DO x=C, B
         dden = dden + Combinatorio(A + B - x, x)
      ENDDO

      PJaccardI = 1.D0 - (dnum / dden)

      END
C
C -------------------------------------------------------------------------
C


      DOUBLE PRECISION Function PJaccardActual(A, B, C)
      INTEGER A, B, C


      If (A + B .NE. C) PJaccardActual = DFLOAT(C) / DFLOAT(A + B - C)
      If (C .EQ. -1) PJaccardActual = -2.D0
      
      END

C----------------------------------------
C
C     VALORESCRITICOSINDICEJACCARD
c
      SUBROUTINE ValCIndJaccard(A, B, p1mas, p2mas, p3mas,
     c                          p1menos, p2menos, p3menos, condiciones)
      INTEGER A, B, CONDICIONES
      DOUBLE PRECISION p1mas, p2mas, p3mas, p1menos, p2menos, p3menos

C'
C'  condiciones = True   1 --> Irreversible
C'              = False  2 --> Reversible
C'
C' A Número de atributos presentes en la zona A
C' B Número de atributos presentes en la zona B
C' C Número de atributos presentes en la zona A y en la zona B
C'
C' N Número total de atributos: N = A + B - C
C'
C' A >= B
C' Dados A y B, obtenemos las probabilidades superiores a los niveles de
C' confianza 0.05 (p1mas), 0.01 (p2mas) y 0.001 (p3mas)
C'
C'''''''''''''''''''''''''''''''''''''''''''''''''''''

      DOUBLE PRECISION dnum, dden, pmas, pmenos
      Integer C
      DOUBLE PRECISION Combinatorio, PJaccardActual
C'
C'
C'    A, B, p1mas, p2mas, p3mas
C'
C'   10;7;0.7;0.;0.
C'   10;8;0.8;0.;0.
C'   10;9;0.7272727272727273;0.9;0.
C'   10;10;0.8181818181818182;0.8181818181818182;1.
C'   65;63;0.4883720930232558;0.5238095238095238;0.5609756097560976
C'   65;64;0.4827586206896552;0.5176470588235295;0.573170731707317
C'   65;65;0.4772727272727273;0.5294117647058824;0.5662650602409639
C
C'  A, B, p1menos, p2menos, p3menos
C'  65;63;0.29292929292929293;0.2549019607843137;0.23076923076923078
C'  65;64;0.29;0.2647058823529412;0.22857142857142856
C'  65;65;0.2871287128712871;0.2621359223300971;0.22641509433962265
C'  66;1;0.;Null;Null
C'  66;2;0.014925373134328358;0.;0.
C'
C''''''''''''''

      dnum = 0.D0
      dden = 0.D0
      pmas = 0.D0
      p1mas = 0.D0
      p2mas = 0.D0
      p3mas = 0.D0
      p1menos = -1.D0
      p2menos = -1.D0
      p3menos = -1.D0

Case True    '  Condiciones irreversibles
      IF (condiciones.eq.1) THEN

      DO k = 0, B
         dden = dden + Combinatorio(A + B - k, k)
      ENDDO

      pmas = 1.D0
      pmenos = 1.D0 / dden

      DO C = 1, B
         pmas = 1.D0 - (dnum / dden)

C' Valores superiores
           If (pmas .LT. 0.05 .And. p1mas .EQ. 0)
     c               p1mas = PJaccardActual(A, B, C)
           If (pmas .LT. 0.01 .And. p2mas .EQ. 0)
     c               p2mas = PJaccardActual(A, B, C)
           If (pmas .LT. 0.001 .And. p3mas .EQ. 0)
     c               p3mas = PJaccardActual(A, B, C)

C' Valores inferiores
           If (pmenos .GT. 0.05.And.p1menos.EQ.-1)
     c               p1menos = PJaccardActual(A, B, C - 2)
           If (pmenos .GT. 0.01.And.p2menos.EQ.-1)
     c               p2menos = PJaccardActual(A, B, C - 2)
           If (pmenos .GT. 0.001.And.p3menos.EQ.-1)
     c               p3menos = PJaccardActual(A, B, C - 2)

           dnum = dnum + Combinatorio(A + B - C, C)
           pmenos = dnum / dden

      ENDDO

C'
C' Casos extremos para los valores inferiores
      If (pmenos .GT. 0.05 .And. p1menos .eq. -1)
     c           p1menos = PJaccardActual(A, B, C - 2)
      If (pmenos .GT. 0.01 .And. p2menos .eq. -1)
     c           p2menos = PJaccardActual(A, B, C - 2)
      If (pmenos .GT. 0.001 .And. p3menos .eq. -1)
     c           p3menos = PJaccardActual(A, B, C - 2)

      If (pmenos .GT. 0.05 .And. p1menos .eq. -1)
     c           p1menos = PJaccardActual(A, B, C - 1)
      If (pmenos .GT. 0.01 .And. p2menos .eq. -1)
     c           p2menos = PJaccardActual(A, B, C - 1)
      If (pmenos .GT. 0.001 .And. p3menos .eq. -1)
     c           p3menos = PJaccardActual(A, B, C - 1)

      END IF
Case False  ' Condciones reversibles

      END
      
      


