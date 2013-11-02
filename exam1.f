      PROGRAM EXAM1

      IMPLICIT NONE

      REAL :: R(10), A(10), B(10), TEMP
      INTEGER :: I, MAIN_SELECTOR, AMNT_ENTERED

    1 FORMAT (' ', 3X, I2, 7X, F6.2, 5X, F6.2, 4X, F6.2)
    2 FORMAT (2X, 'Element', 4X, 'Array A', 4X, 'Array B')
    3 FORMAT (2X, 'Element', 7X, 'Result')
    4 FORMAT (' ', 3X, I2, 9X, F9.2)

      PRINT *, ''
      PRINT *, ''
      PRINT *, 'E X A M  1 '
      PRINT *, ''
      PRINT *, 'WHAT WOULD YOU LIKE TO DO?'
      PRINT *, ''

      MAIN_SELECTOR = 0

      DO WHILE (MAIN_SELECTOR <7)
      PRINT *, '             ****************************************'
      PRINT *, '                            M  E  N  U '
      PRINT *, '                  1 - Enter Arrays'
      PRINT *, '                  2 - Display Arrays'
      PRINT *, '                  3 - Sum Arrays'
      PRINT *, '                  4 - Multiply Arrays'
      PRINT *, '                  5 - Display Resulting Array'
      PRINT *, '                  6 - Quit'
      PRINT *, '             ****************************************'
      READ (*,*) MAIN_SELECTOR
      
      SELECT CASE (MAIN_SELECTOR)

       CASE (1)
        
        PRINT *, 'Enter the data for the first array:'
        DO I=1, 10
         READ (*,*) TEMP
          IF (TEMP .EQ. -999) THEN
           AMNT_ENTERED = I-1
           EXIT
          END IF
          IF (temp .NE. -999) THEN
           A(I) = TEMP
           AMNT_ENTERED = 10      
          END IF
        END DO 

        PRINT *, 'Enter the data for the second array:'
        DO I=1, AMNT_ENTERED
         READ (*,*) B(I)
        END DO

       CASE (2)
        PRINT *, ''
        PRINT 2
        PRINT *, '-------------------------------'
        DO I=1, AMNT_ENTERED
         PRINT 1, I, A(I), B(I)
        END DO
        PRINT *, ''

       CASE (3)
        DO I=1, AMNT_ENTERED
         R(I) = A(I) + B(I)
        END DO

       CASE (4)
        DO I=1, AMNT_ENTERED
         R(I) = A(I)*B(I)
        END DO

       CASE (5)
         PRINT 3
         PRINT *, '-------------------------'
         DO I=1, AMNT_ENTERED
          PRINT 4, I, R(I)
         END DO

      CASE(6)
       STOP

      CASE DEFAULT 
       STOP
          
       END SELECT
         
      END DO

      END PROGRAM