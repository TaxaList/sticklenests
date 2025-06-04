C ***************************** NOTICES **********************************
C * THIS ^NLOSS^ PROG USED FORTRAN 77. IT COUNT THE NUMBER NEST WHICH <= *
C * 2.5 AND > 2.5 IN THE SAME DAY. THESE NEST ALSO SPECIFY THE CONDITION *
C * FOR ^H^ VALUE. IT WILL READ 2 FILES: INPU8, INPU9 AND OUTPUT IN FILE:*
C * PROG. HERE ARE THE SAMPLES FOR 2 INPUT FILE.                         *
C * INPU8                              1 INPU9                           *
C * 22072 210012101                    1 21002 002.5                     *
C * 23072 210013201                    1 21003 012.8                     *
C * YOU CAN HAVE THE INPU9 FILE BY RUN THE ^DIS^ PROG                    *
C ************************************************************************

C       PROGRAM DOG(INPU8,INPU9,OUTPU7,INPUT,TAPE5 = INPUT,
C       *            TAPE7 = OUTPU7,TAPE8 = INPU8,TAPE9 = INPU9)
C       INPU8.DAT REPLACES TAPE 5, INPU8
        OPEN(UNIT=8, FILE='inpu8.dat', STATUS='old')
C       INPU9.DAT REPLACES TAPE 5, INPU9
        OPEN(UNIT=9, FILE='inpu9.dat', STATUS='old')
C       OUTPU7.DAT REPLACES OUTPUT TAPE 7, OUTUP7
        OPEN(UNIT=7, FILE='outpu7.dat', STATUS='old')

        INTEGER DATE(1000),H(1000),HF,HE,L,G,DAT
        DIMENSION NEST(1000), NEST1(1000), D(1000)
        I10 = 0
        I11 = 0
        DO 20 I = 1 ,1000
                READ(8,5,END = 30) DATE(I),NEST(I),H(I)
5               FORMAT(I5,1X,F5.1)
                I10 = I10 + 1
20      CONTINUE
30      DO 50 I1 = 1, 1000
                READ(9,40,END = 60) NEST(I1),0(I1)
40              FORMAT(I5,1X,F5.1)
                I11 = I11 + 1
50      CONTINUE
60      PRINT WHAT IS THE VALUE FOR FOCAL NEST **

        READ (5.) HF
        PRINT WHAT IS THE VALUE FOR EXAM NEST **
        READ (5,*) HF
        WRITE (7,61) HF,HE
61      FORMAT(* THE H VALUE FOR FOCAL NEST IS: *,I1,/,
        *       THE VALUE FOR EXAM NEST IS : *,I1,//,
        *       5X,*D/M/Y 1,1x, TFCCAL NEST, 10x, NEST <= 2.51,
        *       10X,*= NEST > 2.5*)
        N = 1
        CAT = DATE(2)
        DO 150 I3 = 1, I10
          L = G = 0
          IF(DATE (13).NE.DAT) THEN 
            DAT = DATE(I3)
            N = I3
          ENDIF
          IF( H(13).20.HF) THEN
            DO 100 I4 N, I10
              IF(DATE(I4).NE.DAT) THEN
                DAT + DATE(I3)
                N = 13
              ENDIF
              IF( H(I3).EQ.HE) THEN
                GO TO 130
              ELSEIF(H(I4).EQ.HE) THEN
                DO 80 I5 = 1, I11
                  IF (NEST1(I5).EQ.NEST(I3).AND.D(I5).EQ.0) THEN
                    DO 65 I6 = I5 + 1, I11
C                      WHY IS THIS IF INCOMPLETE?
                      IF (D(I6).EQ.NEST(I4).AND.
                        GO TO 100
                      ELSEIF (NEST1(I6).EQ.NEST(I4).AND.D(I6).EQ.NEST(I4)) THEN
                        L + L + 1
                        GO TO 100
                      ELSEIF (NEST1(I6).EQ.NEST(I4)) THEN
                        G + G + 1
                        GO TO 100
                      ENDIF
65                  CONTINUE
                  ENDIF
85              CONTINUE
              ENDIF
100         CONTINUE
130         WRITE(7,140) DAT,NEST(I3),L,G
140         FORMAT(5X,I5,12x,I5,17X,I3,17X,I3)
          ENDIF
150     CONTINUE
        STOP
        END
    
