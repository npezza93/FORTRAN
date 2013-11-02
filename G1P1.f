      PROGRAM HERON

      IMPLICIT NONE

*     Initializing variables
      REAL :: A,B,C, PERIMETER, S, AREA
      LOGICAL :: COND1, COND2, COND3
      INTEGER :: REPEAT = 1

*       Header
      PRINT *, 'WELCOME TO HERONS FORMULA'
      PRINT *, 'PROGRAM 1 BY GROUP 1'
      PRINT *, 'NICK PEZZA AND BRANDON ARNOLD'

*       Loop to repeat if the user desires
      DO WHILE (REPEAT == 1 )

*       Getting the three sides of the triangle
      PRINT *, 'TYPE IN THE LENGTHS OF THE SIDES OF THE TRIANGLE (SEPARA 
     &TE BY A SPACE) ' 
      READ (*,5) A,B,C
    5 FORMAT ( F3.1, 1X, F3.1, 1X, F3.1 )

*       Testing the three conditions
      COND1 = A+B>C
      COND2 = A+C>B
      COND3 = B+C>A

*       Tests to make sure all three conditions are true
      IF (COND1 .AND. COND2 .AND. COND3 ) THEN 

*       If all true finds the perimeter and area
	WRITE (*,6) A,B,C
    6 	FORMAT (1X, 'THE LENGTHS ', F3.1, ', ', F3.1, ', AND ', F3.1, 
     & 		' FORM A TRIANGLE' )
      	PERIMETER = A+B+C
      	WRITE (*,7) PERIMETER
    7 	FORMAT (1X, 'THE PERIMETER IS: ', F3.1, ' UNITS' )
    	S= (A+B+C)/2
    	AREA=SQRT(S*(S-A)*(S-B)*(S-C))
      	WRITE (*,8) AREA
    8      FORMAT (1X, 'THE AREA OF THIS TRIANGLE IS ', F6.3 )
      ELSE

*             If the sides dont form a triangle 
      	WRITE (*,10) A,B,C
  10 	FORMAT (1X, 'THE LENGTHS ', F3.1, ', ', F3.1, ', AND ', F3.1, 
     & 		' DO NOT FORM A TRIANGLE' )

      END IF

*       Asks the user if they want to try again
      PRINT *, 'TRY AGAIN? (1 FOR YES OR 2 FOR NO)'
      READ (*,9) REPEAT
    9 FORMAT ( I1 ) 

      END DO

      END PROGRAM HERON