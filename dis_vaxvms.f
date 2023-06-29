C ********************************************************************
C * THIS DIS PROG USED FORTRAN 77. IT COMPUTE THE DISTANCE BETWEEN   *
C * NESTS. IT WILL READ THE FILE NAMELY IS: INPU4, AND OUTPUT IN OUTP8
C * REMENBER RENAME INPUT FILE LIKE ABOVE. HERE IS A SAMPLE OF INPUT *
C * FILE:           11001 020 069                                    *
C *                 11001 022 167                                    *
C ********************************************************************

c     PROGRAM DOG(INPU4, OUTP8, TAPE4, INPU4, TAPE8 = OUTPU8)
      DIMENSION NEST(2000),X(2000),Y(2000)
	  open(unit = 4,file = 'paccoo1.dat',status = 'old')
	  open(unit = 8,file = 'paccoor1.dat',status = 'new')
      I1 = 0
      DO 10 I=1, 2000
	    READ (4,5, END = 15 ) NEST(I),X(I),Y(I)
5		FORMAT ( I5, 2(1X,F3.1)
		I1 = I1 + 1
10	  CONTINUE
15	  DO 100 I2 = 1, I1
		WRITE ( 8,20 ) NEST(i2)
20		FORMAT ( I5, 3X,'0.0000',10X,'THIS IS A NEW FOCAL NEST')
	  DO 50 I3 = 1, I1
	    IF ( I3.EQ.I2 ) GO TO 50
		DIS = ((X(I2-X(I3))**2 + (Y(I2)-Y(I3))**2)**0.5
		IF(DIS.LE.1.0) WRITE ( 8,25 ) NEST(I3), DIS
25		FORMAT ( I5, 1X, F5.1)
50	    CONTINUE
100	   CONTINUE
	   CLOSE = 4
	   CLOSE = 8
	   STOP
	   END