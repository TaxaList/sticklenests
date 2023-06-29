C ********************************************************************
C * THIS DIS PROG USED FORTRAN 77. IT COMPUTE THE DISTANCE BETWEEN   *
C * NESTS. IT WILL READ THE FILE NAMELY IS: INPU4, AND OUTPUT IN OUTP8
C * REMENBER RENAME INPUT FILE LIKE ABOVE. HERE IS A SAMPLE OF INPUT *
C * FILE:           11001 020 069                                    *
C *                 11001 022 167                                    *
C ********************************************************************

      PROGRAM DOG(INPU4, OUTP8, TAPE4, INPU4, TAPE8 = OUTPU8)
      DIMENSION NEST(2000),X(2000),Y(2000)
      I1 = 0
      DO 10 I=1, 2000
        
