      SUBROUTINE GAUSS_JORDAN (N,A,B, ERROR)
        INTEGER, INTENT(IN) :: N
        REAL, INTENT(INOUT), DIMENSION(N,N) :: A
        REAL, INTENT(INOUT), DIMENSION(N) :: B
        INTEGER, INTENT(IN) :: N
        INTEGER, INTENT(OUT) :: ERROR
        REAL, PARAMETER :: EPSILON = 1.0E-6
        REAL :: FACTOR, TEMP
        INTEGER :: IROW, IPEAK, JROW, KCOL
        
        DO IROW=1, N
          
          IPEAK=IROW
          DO JROW=IROW+1,N
           IF (ABS(A(JROW,IROW)) .GT. ABS(A(IPEAK, IROW))) THEN
             IPEAK = JROW
           END IF
          END DO
          
          IF (ABS(A(IPEAK,IROW)) .LT. EPSILON) THEN
           ERROR=1
           RETURN
          END IF
         
          IF(IPEAK .NE. IROW) THEN
           DO KCOL=1, N
            TEMP= A(IPEAK,KCOL)
            A(IPEAK,KCOL) = A(IROW,KCOL)
            A(IROW,KCOL)=TEMP
           END DO
           TEMP=B(IPEAK)
           B(IPEAK)=B(IROW)
           B(IROW)=TEMP
          END IF

 
      PROGRAM GAUSS-JORDAN
      
      IMPLICIT NONE
 
      LOGICAL :: FLAG
      CHARACTER*10 IFILENAME
      CHARACTER*10 OFILENAME
      INTEGER :: IERROR, OERROR, OSELECTOR, N
      REAL :: X(10,10), S(1,10)

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
      READ (1, *) N
       IF (N .LE. 10 .AND. N .GE. 0) THEN
        DO I=1, N
         READ (1, *, IOSTAT=READING) (X(I,J), J=1, N)
        END DO
    
      CALL SUBROUTINE GAUSS_JORDAN(N,X,S)
       



