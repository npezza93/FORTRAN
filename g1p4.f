      PROGRAM MATRIX_ADDITION

!      The Matrix Add Program
!      Program #4
!      Nick Pezza and Brandon Arnold 
!      Group 1

      IMPLICIT NONE

!     Variable initializations
      INTEGER :: IERROR, OERROR, OSELECTOR, NUM_MATRIX
      INTEGER :: N, M,I , J, READING, X
      CHARACTER*10 IFILENAME
      CHARACTER*10 OFILENAME
      REAL TEMP(10,10)
      REAL SUM(10,10)
      LOGICAL :: FLAG

!       format statements
    1 FORMAT (I2)
   10 FORMAT (10(F3.1, X))
   11 FORMAT (10(F5.2, X))
   20 FORMAT (' ', 'MATRIX ', I2)
   21 FORMAT (' ', '---------')
   30 FORMAT (' ', 'SUM OF ALL ', I2, ' MATRICES')
   31 FORMAT (' ', '---------')

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
           OPEN (UNIT=2, FILE=OFILENAME, STATUS='REPLACE', ACTION='WRITE
     &', IOSTAT=OERROR, FORM = 'FORMATTED')
         CASE (3)
           FLAG = .FALSE.
         CASE DEFAULT
           FLAG = .FALSE.
          END SELECT
          END DO

!      Skips this entire section if the user quits
      IF (FLAG .EQV. .TRUE.) THEN

!      Reads in the number of matrices, columns, and rows
      READ (1, 1) NUM_MATRIX
      READ (1, *) N, M

!       Sets sum matrix to zero 
      DO I=1, 10
       DO J=1, 10
        SUM(I,J) = 0
       END DO
      END DO
      
!        Do the amount of matrices there are
      DO X=1, NUM_MATRIX

!        Grabs the curent matrix
      DO I=1,N
       READ (1, 10, IOSTAT=READING) (TEMP(I,J), J=1, M)
      END DO

!       Prints the current matrix to the output file
      WRITE (2,20) X
      WRITE (2,21)
      DO I=1, N
       WRITE (2, 11) (TEMP(I,J), J=1, M)
      END DO
      
!         Calculates the sum  
      DO I=1,N
       DO J=1, M
       SUM(I,J) = SUM(I,J) + TEMP(I,J)
       END DO
      END DO

      END DO

!         Writes the sum matrix to the output with fancy header
       X = X-1
      WRITE (2, 30) X
      WRITE (2, 31)
      DO I=1, N
         WRITE (2, 11) (SUM(I,J), J=1, M)
      END DO
      CLOSE (2)

!       Closes the files if user opened and quit
      ELSE
        CLOSE (1)
        CLOSE (2)
      END IF

      END PROGRAM MATRIX_ADDITION