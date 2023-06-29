C *************************************************************************
C * THIS RANK PROG WILL RANK THE NESTS BY DISTANCE. IT WILL READ INPUT    *
C * NAMELY INPU AND OUTPUT IN OUTP FILE REMEMBER RENAME THE INPUT FILE    *
C * FILE BEFORE RUN THE PROG AND ALSO SAVE OUTP FILE. HERE IS THE SAMPLE  *
c * OF INPU FILE                                                          *
C * 1064 062 034                                                          *
C * 1065 068 056                                                          *      
C *************************************************************************
      PROGRAM D(INPU,OUTP,TAPE1 = INPU, TAPE2 = OUTP  )
      INTEGER X(1000),Y(1000)
      DIMENSION NEST(1000),NEST1(1000),DIS(1000)

      I10 = 1
      DO 10 I = 1, 1000
      READ(1,5, END = 20 ) NEST(I), X(1000), X(I), Y(I)
 5    FORMAT(I4,2,(1X,I3))
      I10 = I10 + 1
      NEST1(I) = NEST(I)
 10   CONTINUE
 20   DO 100 I1 = 1, I10 - 1
        DO 40 I2 = 1, I10
          DIS(I2) = ((X(I1) - X(I2))**2 + (Y(I1)-Y( I2))**2)**0.5
 40     CONTINUE
        K = 0
 50   IF(K.EQ.0) THEN
          K = 1
        DO 60 I3 = 1, I10 - 1
		 IF (DIS(I3).GT.DIS(I3+1)) THEN
		  TEMP = DIS(I3)
		  TEMP1 = NEST1(I3)
		  DIS(I3) = DIS(I3+1)
		  NEST1(I3) = NEST1(I3+1)
		  DIS(I3+1) = TEMP
		  NEST1(I3+1) = TEMP1
		  K = 0
		 ENDIF
60		CONTINUE
			GO TO 50
	  ENDIF
	   WRITE (2,65) NEST1(1)
65	   FORMAT(1X,I4,20X,^THIS IS A NEW FOCAL NEST^)
	   NEST1(1) = NEST(1)
	    DO 80 I4 = 2, I10
		  WRITE(2,70) NEST1(I4), DIS(I4)
80		CONTINUE
		  WRITE(2, 85)
		  FORMAT(IX,^OOOO^)
100	  CONTINUE
	  STOP
	  END