!     The Graphing Program     
!     Program 6  
!     Nick Pezza and Brandon Arnold 
!     Group 1

      PROGRAM XYGRAPH
      
      IMPLICIT NONE

!     Variable initializations
      INTEGER :: IERROR, OERROR, OSELECTOR, NUM_PTS, READING, I, J, K
      INTEGER :: IPTR, Z, INDEX, Y_POS
      CHARACTER*10 IFILENAME
      CHARACTER*10 OFILENAME
      LOGICAL :: FLAG, FOUND
      REAL :: PTS(100,2), TEMP (1,2)
      REAL :: X_MIN, X_MAX, X_STEP, X, AMNT_X_STEPS
      REAL :: X_SCALE_ARRAY
      REAL :: Y_PTS(100), Y_TEMP, Y_MIN, Y_MAX, Y_STEP, Y
      REAL :: Y_SCALE, Y_SCALE_ARRAY(7)
      CHARACTER :: LINE(60)

   10 FORMAT ('The minimum X value is: ', F7.3)
   11 FORMAT ('The maximum X value is: ', F7.3)
   12 FORMAT ('The X step size is: ', F7.3)
   13 FORMAT ('The minimum Y value is: ', F7.3)
   14 FORMAT ('The maximum Y value is: ', F7.3)
   15 FORMAT ('The Y step size is: ', F7.3)
   99 FORMAT (' ')
   98 FORMAT ('__________________________________')
   20 FORMAT (2(5X, F7.3, 3X))
   30 FORMAT (7(F10.4))
   40 FORMAT (10X, 60(A1))
   41 FORMAT (F10.4, 60(A1))
 

      FLAG = .TRUE.

!      Grabs input files name and checks existence 
      PRINT *, 'What is the name of the input file?'
      READ (*,*) IFILENAME
      OPEN (UNIT=1, FILE= IFILENAME, STATUS='OLD', ACTION='READ', 
     &     IOSTAT=IERROR)

!       If the file doesnt exist asks again or quit
      DO  WHILE (IERROR .NE. 0 .AND. FLAG .EQV. .TRUE.) 
        PRINT *, 'That file does not exists. Enter a different file name
     & or QUIT.'
        READ (*,*) IFILENAME
        IF (IFILENAME .EQ. 'QUIT' .OR. IFILENAME .EQ. 'quit') THEN
           FLAG = .FALSE.
        END IF
        OPEN (UNIT=1, FILE= IFILENAME, STATUS='OLD', ACTION='READ', 
     &     IOSTAT=IERROR)
      END DO

!       Grabs the output files name and tests existence
      IF (FLAG .EQV. .TRUE.) THEN
      PRINT *, 'What is the name of the output file?'
      READ (*,*) OFILENAME
      OPEN (UNIT=2, FILE=OFILENAME, STATUS='NEW', ACTION='WRITE',
     &     IOSTAT=OERROR, FORM = 'FORMATTED')
      END IF

!       If the output exists asks again or overwrite or quit
      DO WHILE (OERROR .NE. 0 .AND. FLAG .EQV. .TRUE.)
        PRINT *, 'That file already exists:'
        PRINT *, '      1- Enter new file name'
        PRINT *, '      2- Overwrite existing file'
        PRINT *, '      3- Quit'
        READ (*,*) OSELECTOR
        SELECT CASE (OSELECTOR)
         CASE(1)
           READ (*,*) OFILENAME
           OPEN (UNIT=2, FILE=OFILENAME, STATUS='NEW', ACTION='WRITE',
     &     IOSTAT=OERROR)
         CASE (2)
           OPEN (UNIT=2,FILE=OFILENAME,STATUS='REPLACE', ACTION='WRITE',
     & IOSTAT=OERROR, FORM = 'FORMATTED')
         CASE (3)
           FLAG = .FALSE.
         CASE DEFAULT
           FLAG = .FALSE.
          END SELECT
          END DO

!      Skips this entire section if the user quits
      IF (FLAG .EQV. .TRUE.) THEN
!      Reads in the number of x,y points
      READ (1, *) NUM_PTS
       IF (NUM_PTS .LE. 100 .AND. NUM_PTS .GT. 0) THEN
         DO I=1,NUM_PTS
          READ (1, *, IOSTAT=READING) (PTS(I,J), J=1, 2)
         END DO

!      Sorts all points by X 
      DO I=1, NUM_PTS-1
       IPTR = I
       DO J=I+1, NUM_PTS
        IF (PTS(J,1) < PTS(IPTR,1)) THEN
         IPTR = J
        END IF
       END DO
      
      IF (I .NE. IPTR) THEN 
         TEMP(1,1) = PTS(I,1)
         TEMP(1,2) = PTS(I,2)
         PTS(I,1) = PTS(IPTR,1)
         PTS(I,2) = PTS(IPTR,2)
         PTS(IPTR,1) = TEMP(1,1)
         PTS(IPTR,2) = TEMP(1,2)
      END IF
      END DO
!       End of sorting

!       Finds the x step
      X_STEP = 100000000
      DO I=1 , NUM_PTS-1
        J=I+1
        X = PTS(J,1) - PTS(I,1)
        DO WHILE (X .GT. 0 .AND. X_STEP .GT. X)
         X_STEP = X
        END DO
      END DO
!      End of finding x step

      X_MIN = PTS(1,1)
      X_MAX = PTS(NUM_PTS, 1)
      WRITE (2,98)
      WRITE (2,10) X_MIN
      WRITE (2,11) X_MAX
      WRITE (2,12) X_STEP
      WRITE (2,98)
      WRITE (2,99)

!     Load array of Y's to sort
      DO I=1, NUM_PTS
       Y_PTS(I) = PTS(I,2)
      END DO
!     End of loading Y array

!     Sorting Y points
      DO I=1, NUM_PTS-1
       IPTR = I
       DO J=I+1, NUM_PTS
        IF (Y_PTS(J) < Y_PTS(IPTR)) THEN
         IPTR = J
        END IF
       END DO
      
      IF (I .NE. IPTR) THEN 
         Y_TEMP = Y_PTS(I)
         Y_PTS(I) = Y_PTS(IPTR)
        Y_PTS(IPTR) = Y_TEMP
      END IF
      END DO
!     End of sorting Y points

!       Finds the y step
      Y_STEP = 100000000
      DO I=1 , NUM_PTS-1
        J=I+1
        Y = Y_PTS(J) - Y_PTS(I)
        DO WHILE (Y .GT. 0 .AND. Y_STEP .GT. Y)
         Y_STEP = Y
        END DO
      END DO
!      End of finding y step

      Y_MIN = Y_PTS(1)
      Y_MAX = Y_PTS(NUM_PTS)
      WRITE (2,98)
      WRITE (2,13) Y_MIN
      WRITE (2,14) Y_MAX
      WRITE (2,15) Y_STEP
      WRITE (2,98)
      WRITE (2,99)

      WRITE (2,98)
      WRITE (2,*) 'The data points entered are:'
      DO I=1,NUM_PTS
       WRITE (2, 20) (PTS(I,J), J=1, 2)
      END DO
      WRITE (2,98)
      WRITE (2,99)
      WRITE (2,99)

!     Determines the Y Scale array
      Y_SCALE = (Y_MAX - Y_MIN)/(6)
      Y_SCALE_ARRAY(1) = Y_MIN
      DO I=1, 6
       Y_SCALE_ARRAY(I+1) = (Y_MIN + (Y_SCALE*I))
      END DO
!     End of the y scale array

!     Determines the X-scale and adJUsts the X-step if necassary 
      AMNT_X_STEPS = (X_MAX-X_MIN)/(X_STEP)
      DO WHILE (AMNT_X_STEPS .GT. 200) 
       X_STEP = X_STEP*2
       AMNT_X_STEPS = (X_MAX-X_MIN)/(X_STEP)
      END DO
      DO WHILE (AMNT_X_STEPS .LT. 20)
       X_STEP = X_STEP/2
       AMNT_X_STEPS = (X_MAX-X_MIN)/(X_STEP)
      END DO
!     End of determining the amount of x steps
       
       
!      Writing the graph
      WRITE (2, 30) (Y_SCALE_ARRAY(I), I=1, 7)
      DO I=0, CEILING(AMNT_X_STEPS)
       DO J=1, 60
        LINE(J) = " "
       END DO
       IF ( I .EQ. 0) THEN
        DO J=1, 60
         Z = MOD(J,10)
         IF (Z .EQ. 0 .OR. J .EQ. 1) THEN
          LINE(J) = '+'
         ELSE
          LINE(J) = '-'
         END IF
        END DO
       END IF
       IF (MOD(I,5) .EQ. 0) THEN
        LINE(1) = "+"
       ELSE
        LINE(1) = "|"
       END IF
       X=X_MIN+I*X_STEP

       INDEX = 1
       DO WHILE (INDEX .LE. NUM_PTS)
        FOUND = .FALSE.
        DO WHILE (.NOT. FOUND .AND. INDEX .LE. NUM_PTS)
         IF (PTS(INDEX,1) .GT. X-(X_STEP/2) 
     &                    .AND. PTS(INDEX,1) .LE. X+(X_STEP/2)) THEN
           FOUND = .TRUE.
         ELSE
          INDEX=INDEX+1
         END IF
        END DO
        IF (INDEX .LE. NUM_PTS) THEN
         Y_POS = NINT((PTS(INDEX,2)-Y_MIN)/(Y_MAX-Y_MIN)*59)+1
         IF (LINE(Y_POS) .EQ. " " .OR. LINE(Y_POS) .EQ. "+" .OR. 
     &            LINE(Y_POS) .EQ. "|" .OR. LINE(Y_POS) .EQ. "-" ) THEN
          LINE(Y_POS) = "*"
          INDEX=INDEX+1        
         ELSE
          LINE(Y_POS) = "o"
          INDEX=INDEX+1
         END IF
        END IF
       END DO    
       IF (MOD(I,5) .EQ. 0) THEN
        WRITE (2,41) X, (LINE(K), K=1, 60)
       ELSE
        WRITE (2,40) (LINE(K), K=1, 60)
       END IF
      END DO
         ELSE
           PRINT *, 'ERROR: Too many points or too few'
           CLOSE (1)
           CLOSE (2)
       END IF
      END IF
      CLOSE(1)
      CLOSE(2)

      END PROGRAM
