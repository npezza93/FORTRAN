      SUBROUTINE ADDITION(ROW, COLUMN)
       INTEGER :: ROW, COLUMN
       INTEGER :: A(ROW,COLUMN), B(ROW,COLUMN), SUM(ROW, COLUMN)
       INTEGER :: I, J 

   11 FORMAT (20(I2, X)) 

       PRINT *, 'ENTER MATRIX A:'
       DO I=1, ROW 
        READ (*,*) (A(I,J), J=1, COLUMN) 
       END DO

       PRINT *, 'ENTER MATRIX B:'
       DO I=1, ROW 
        READ (*,*) (B(I,J), J=1, COLUMN) 
       END DO

       DO I=1,ROW
        DO J=1, COLUMN
         SUM(I,J) = A(I,J) + B(I,J)
        END DO
       END DO

       PRINT *, 'MATRIX A:'
       DO I=1,ROW
        DO J=1, COLUMN
         WRITE (*,11) A(I,J)
        END DO
       END DO
 
       PRINT *, 'MATRIX B:'
       DO I=1,ROW
        DO J=1, COLUMN
         WRITE (*,11) B(I,J)
        END DO
       END DO
 
       PRINT *, 'THE SUM OF THE MATRICES IS:'
       DO I=1,ROW
        DO J=1, COLUMN
         WRITE (*,11) SUM(I,J)
        END DO
       END DO

       RETURN
      END SUBROUTINE ADDITION

      SUBROUTINE SUBTRACTION (ROW, COLUMN)
       INTEGER :: ROW, COLUMN
       INTEGER :: A(ROW,COLUMN), B(ROW,COLUMN), DIFFERENCE(ROW, COLUMN)
       INTEGER :: I, J 

   11 FORMAT (20(I2, X)) 

       PRINT *, 'ENTER MATRIX A:'
       DO I=1, ROW 
        READ (*,*) (A(I,J), J=1, COLUMN) 
       END DO

       PRINT *, 'ENTER MATRIX B:'
       DO I=1, ROW 
        READ (*,*) (B(I,J), J=1, COLUMN) 
       END DO

       DO I=1,ROW
        DO J=1, COLUMN
         DIFFERENCE(I,J) = A(I,J) - B(I,J)
        END DO
       END DO

       PRINT *, 'MATRIX A:'
       DO I=1,ROW
        DO J=1, COLUMN
         WRITE (*,11) A(I,J)
        END DO
       END DO
       
       PRINT *, 'MATRIX B:'
       DO I=1,ROW
        DO J=1, COLUMN
         WRITE (*,11) B(I,J)
        END DO
       END DO
       
       PRINT *, 'THE DIFFERENCE OF THE MATRICES IS:'
       DO I=1,ROW
        DO J=1, COLUMN
         WRITE (*,11) DIFFERENCE(I,J)
        END DO
       END DO

       RETURN
      END SUBROUTINE SUBTRACTION

      SUBROUTINE TRANSPOSE (ROW, COLUMN)
       INTEGER :: ROW, COLUMN
       INTEGER :: A(ROW,COLUMN), Z(COLUMN, ROW)
       INTEGER :: I, J 

   11 FORMAT (20(I2, 1X))  

       PRINT *, 'ENTER YOUR MATRIX:'
       DO I=1, ROW 
        READ (*,*) (A(I,J), J=1, COLUMN) 
       END DO

       PRINT *, 'THE TRANSPOSE OF:'
       DO I=1,ROW
          WRITE (*,11) (A(I,J), J=1, COLUMN) 
       END DO

       DO I=1,ROW
        DO J=1, COLUMN
          Z(J,I)= A(I,J)
        END DO
       END DO

       PRINT *, 'IS:'
       DO I=1,COLUMN
          WRITE (*,11) (Z(I,J), J=1, ROW)
       END DO 

       RETURN
       END SUBROUTINE TRANSPOSE      

      SUBROUTINE DOT(ROW, COLUMN, DOT_2_COLUMN)
         INTEGER :: ROW, COLUMN, DOT_2_COLUMN, I, J, K
         INTEGER :: A(ROW, COLUMN), B(COLUMN, DOT_2_COLUMN)
         INTEGER :: DOT_PROD(ROW, DOT_2_COLUMN)
         
   11 FORMAT (20(I2, 1X))
   12 FORMAT (20(I4, 1X))
   20 FORMAT ('ENTER THE 2nd MATRIX(',I2, ' x ', I2, '):')
         
         ELEMENTS = ROW*DOT_2_COLUMN

         PRINT *, 'ENTER THE FIRST MATRIX:'
         DO I=1, ROW 
          READ (*,*) (A(I,J), J=1, COLUMN) 
         END DO
         
         PRINT 20, COLUMN, DOT_2_COLUMN
         DO I=1, COLUMN 
          READ (*,*) (B(I,J), J=1, DOT_2_COLUMN) 
         END DO
         
         DO I=1, ROW
          DO J=1, DOT_2_COLUMN
           DOT_PROD(I,J) = 0
          END DO
         END DO

         DO I=1, ROW
          DO J=1, DOT_2_COLUMN
           DO K=1, COLUMN
            DOT_PROD(I,J) = DOT_PROD(I,J) + ((A(I,K))*(B(K,J)))
           END DO
          END DO
         END DO

         PRINT *, 'MATRIX A:'
         DO I=1, ROW 
          WRITE (*,11) (A(I,J), J=1, COLUMN) 
         END DO
         
         PRINT *, 'MATRIX B:'
         DO I=1, COLUMN 
          WRITE (*,11) (B(I,J), J=1, DOT_2_COLUMN) 
         END DO
     
         PRINT *, 'PRODUCT:'
         DO I=1, ROW 
          WRITE (*,12) (DOT_PROD(I,J), J=1, DOT_2_COLUMN) 
         END DO

       END SUBROUTINE DOT
       
      PROGRAM MATRIX_CALCULATOR

      IMPLICIT NONE

      INTEGER :: SELECTION, ROW, COLUMN, DOT_2_COLUMN
      LOGICAL :: FLAG

   21 FORMAT ('THE SECOND MATRIX IS ',I2, ' BY WHAT?')

      FLAG = .TRUE.

      PRINT *, 'WELCOME TO THE MATRIX CALCULATOR'
      PRINT *, ''

      DO WHILE (FLAG .EQV. .TRUE. ) 

      PRINT *, 'WHAT OPERATION WOULD YOU LIKE TO PERFORM?'
      PRINT *, '   1 - ADDITION'
      PRINT *, '   2 - SUBTRACTION'
      PRINT *, '   3 - MULTIPLICATION'
      PRINT *, '   4 - TRANSPOSE'
      PRINT *, '   5 - QUIT'

      READ (*,*) SELECTION

      SELECT CASE (SELECTION)

       CASE (1)
        PRINT *, 'WHAT DIMENSIONS ARE THE MATRICES?'
        PRINT *, 'ROWS?'
        READ (*,*) ROW
        PRINT *, 'COLUMNS?'
        READ (*,*) COLUMN
        CALL  ADDITION(ROW, COLUMN) 

       CASE (2)
        PRINT *, 'WHAT DIMENSIONS ARE THE MATRICES?'
        PRINT *, 'ROWS?'
        READ (*,*) ROW
        PRINT *, 'COLUMNS?'
        READ (*,*) COLUMN
        CALL SUBTRACTION(ROW, COLUMN)

       CASE (3)
        PRINT *, 'WHAT DIMENSION IS THE FIRST MATRIX?'
        PRINT *, 'ROWS?'
        READ (*,*) ROW
        PRINT *, 'COLUMNS?'
        READ (*,*) COLUMN
        PRINT 21, COLUMN
        READ (*,*) DOT_2_COLUMN
        CALL DOT(ROW, COLUMN, DOT_2_COLUMN)        

       CASE (4)
        PRINT *, 'WHAT DIMENSION IS THE MATRIX?'
        PRINT *, 'ROWS?'
        READ (*,*) ROW
        PRINT *, 'COLUMNS?'
        READ (*,*) COLUMN
        CALL TRANSPOSE(ROW, COLUMN)

       CASE (5)
        FLAG = .FALSE.

       CASE DEFAULT
        FLAG = .FALSE.
       
      END SELECT
      END DO
      END PROGRAM 
