      SUBROUTINE G_J (N,A,B, ERROR)
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
          
          DO JROW=1, N
           IF(JROW .NE. IROW) THEN
            FACTOR = -A(JROW,IROW)/A)(IROW,IROW)
            DO KCOL=1, N
             A(JROW,KCOL) = A(IROW,KCOL)*FACTOR+A(JROW,KCOL)
            END DO
            B(JROW) = B(IROW)*FACTOR + B(JROW)
           END IF
          END DO
        END DO
        
        DO IROW=1, N
         B(IROW)=B(IROW)/A(IROW,IROW)
         A(IROW,IROW)=1
        END DO
        
        ERROR =0
      END SUBROUTINE G_J
      
      PROGRAM GAUSS-JORDAN
      
      IMPLICIT NONE
 
      LOGICAL :: FLAG
      INTEGER, PARAMETER :: MAX_SIZE = 10
      CHARACTER*10 IFILENAME
      CHARACTER*10 OFILENAME
      INTEGER :: IERROR, OERROR, OSELECTOR, N, I, J, ERROR
      REAL, DIMENSION (MAX_SIZE, MAX_SIZE) :: A(10,10)
      REAL, DIMENSION (MAX_SIZE) :: B(10)

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
!      Reads in the number of equations
      READ (1, *) N
       IF (N .LE. 10 .AND. N .GE. 0) THEN
        DO I=1, N
         READ (1, *, IOSTAT=READING) (A(I,J), J=1, N), B(I)
        END DO
        
        WRITE(*,"(/,1X,'COEFFICIENTS BEFORE CALL:')")
        DO I=1,N
         WRITE(*,"1X,7F11.4)") (A(I,J), J=1, N), B(I)
        END DO
        
        CALL SUBROUTINE G_J(A, B, MAX_SIZE, N, ERROR)
        
        IF (ERROR .NE. 0) THEN
         WRITE (*, 1010)
 1010  FORMAT(/1X, 'ZERO PIVOT ENCOUNTERED! NO UNIQUE SOLUTION'
        ELSE 
         WRITE (*,"(/,1X, 'COEFFICIENTS AFTER CALL:')")
         DO I=1, N
          WRITE (*,"(1X, 7F11.4)") (A(I,J), J=1,N), B(I)
         END DO
         WRITE (*,"/,1X,'THE SOLUTIONS ARE:')")
         DO I=1, N
          WRITE (*,"(3X,'X(',I2,')= ',F16.6)") I, B(I)
         END DO
         
         END IF
        END IF
       END IF
       END PROGRAM GAUSS_JORDAN
          
         
         
        



