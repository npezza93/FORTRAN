    ! Cash Register program
    ! Nick Pezza and Brandon Arnold
    ! Group 1
    ! CSC 306 Fortran

      PROGRAM CASHREGISTER
      
      IMPLICIT NONE
    ! initalizations 
      INTEGER :: QUANTITY(5), I, MAIN_SELECTOR, ORDER_ADD_SELECTOR
      INTEGER :: ORDER_SUB_SELECTOR, SUB_QUANT
      CHARACTER*20 ITEMS(5)
      REAL :: PRICE(5), SUBTOTAL(5), TOTAL, TAX, AMNT_DUE, AMNT_TEND
      REAL :: CHANGE
      CHARACTER*1 STRT_NEW

    ! formats
    1 FORMAT (' ','Selector', 12X,'Item', 16X, 'Quantity', 12X,
     +'Subtotal',12X)
    2 FORMAT (' ', 3X, I1, 12X, A20, 5X, I3, 16X, '$', F6.2, 14X)
    3 FORMAT (' ', 45X, 'Total:', 8X, '$', F6.2)
    4 FORMAT (' ', 47X, 'Tax:', 8X, '$', F6.2)
    5 FORMAT (' ', 40X, 'Amount Due:', 8X, '$', F6.2)
    6 FORMAT (' ',35X, 'Amount Tendered:', 8X, '$', F6.2)
    7 FORMAT (' ', 44X, 'Change:', 8X, '$', F6.2)

    ! set the item names
      ITEMS(1) = 'Royale with Cheese'
      ITEMS(2) = 'Big Kahuna Burger'
      ITEMS(3) = 'Crabby Patty'
      ITEMS(4) = 'Big Mac'
      ITEMS(5) = 'Whopper' 
      
    ! set the prices
      PRICE(1) = 6.42
      PRICE(2) = 8.16
      PRICE(3) = 5.45
      PRICE(4) = 1.49
      PRICE(5) = 2.39
      
    ! zeros quantity and subtotal
      DO I=1, 5
        QUANTITY(I) = 0
        SUBTOTAL(I) = 0
      END DO

      MAIN_SELECTOR = 1

      PRINT *, ''
      PRINT *, ''
      PRINT *, '        W E L C O M E   T O   T H E   B U R G E R   S H    
     +A C K'
      PRINT *, ''
      PRINT *, '                      WHAT WOULD YOU LIKE TO DO?'
      PRINT *, ''

      DO WHILE (MAIN_SELECTOR <5)
      PRINT *, '             ****************************************'
      PRINT *, '                            M  E  N  U '
      PRINT *, '                  1 - Add Items To An Order'
      PRINT *, '                  2 - Subtract Items From An Order'
      PRINT *, '                  3 - Finalize Order'
      PRINT *, '                  4 - Exit'
      PRINT *, '             ****************************************'
      READ (*,*) MAIN_SELECTOR
      
      SELECT CASE (MAIN_SELECTOR)
       
    ! adding items to an order   
       CASE(1)
        ORDER_ADD_SELECTOR = 0
        DO WHILE (ORDER_ADD_SELECTOR < 999)   
        PRINT *, ''
        PRINT 1
        PRINT *, '------------------------------------------------------
     +-----------------------'
        DO I=1, 5
         PRINT 2, I, ITEMS(I), QUANTITY(I), SUBTOTAL(I)
        END DO
        PRINT *, '------------------------------------------------------
     +-----------------------'
        PRINT *, ''
        PRINT *, 'What items would you like to add to the order?(Type 99
     +9 when complete)' 
        READ (*,*) ORDER_ADD_SELECTOR
        IF (ORDER_ADD_SELECTOR < 999) THEN
         QUANTITY(ORDER_ADD_SELECTOR) = QUANTITY(ORDER_ADD_SELECTOR) + 1
         SUBTOTAL(ORDER_ADD_SELECTOR) = QUANTITY(ORDER_ADD_SELECTOR
     +    ) * PRICE(ORDER_ADD_SELECTOR)
         END IF
        END DO

    ! subtracting items from an order 
       CASE (2)
        ORDER_SUB_SELECTOR = 0 
        DO WHILE (ORDER_SUB_SELECTOR < 999)
        PRINT *, ''
        PRINT 1
        PRINT *, '------------------------------------------------------
     +-----------------------'
        DO I=1, 5
         IF (QUANTITY(I) < 0) THEN
             QUANTITY(I) = 0
             SUBTOTAL(I) = 0
          END IF
         PRINT 2, I, ITEMS(I), QUANTITY(I), SUBTOTAL(I)
        END DO
        PRINT *, '------------------------------------------------------
     +-----------------------'
        PRINT *, ''
        PRINT *, 'What item would you like to deduct?
     +(Type 999 when complete)' 
        READ (*,*) ORDER_SUB_SELECTOR
        IF (ORDER_SUB_SELECTOR < 999) THEN
        PRINT *, 'How many would you like to subtract?'
        READ (*,*) SUB_QUANT
         QUANTITY(ORDER_SUB_SELECTOR) = QUANTITY(ORDER_SUB_SELECT
     +    OR) - SUB_QUANT
         SUBTOTAL(ORDER_SUB_SELECTOR) = QUANTITY(ORDER_SUB_SELE
     +    CTOR) * PRICE(ORDER_SUB_SELECTOR)
         END IF
         END DO
        
    ! makes the fancy reciept    
        CASE(3)
         TOTAL = SUBTOTAL(1)+SUBTOTAL(2)+SUBTOTAL(3)+SUBTOT
     +    AL(4)+SUBTOTAL(5)
         TAX = TOTAL * 0.06
         AMNT_DUE = TAX + TOTAL
         PRINT *, ''
         PRINT 1
         PRINT *, '-----------------------------------------------------
     +-----------------------'
         DO I=1, 5
          PRINT 2, I, ITEMS(I), QUANTITY(I), SUBTOTAL(I)
         END DO
         PRINT *, ''
         PRINT *, ''
         PRINT *, ''
         PRINT 3, TOTAL
         PRINT 4, TAX
         PRINT 5, AMNT_DUE
         PRINT *, '-----------------------------------------------------
     +-----------------------'
         PRINT *, ''
         PRINT *, 'Enter the amount tendered'
         READ (*, *) AMNT_TEND
         DO WHILE (AMNT_TEND < AMNT_DUE) 
             PRINT *, "The customer owes you more money! Try again."
             READ (*,*) AMNT_TEND
         END DO
         CHANGE = AMNT_TEND - AMNT_DUE
         
         PRINT *, ''
         PRINT 1
         PRINT *, '-----------------------------------------------------
     +-----------------------'
         DO I=1, 5
          PRINT 2, I, ITEMS(I), QUANTITY(I), SUBTOTAL(I)
         END DO
         PRINT *, ''
         PRINT *, ''
         PRINT *, ''
         PRINT 3, TOTAL
         PRINT 4, TAX
         PRINT 5, AMNT_DUE
         PRINT *, ''
         PRINT 6, AMNT_TEND
         PRINT 7, CHANGE
         PRINT *, '-----------------------------------------------------
     +-----------------------'
          PRINT *, ''
          PRINT *, 'Would you like to start a new order?( y or n)'
          READ (*,*) STRT_NEW
          IF (STRT_NEW == 'y' .OR. STRT_NEW =='Y' ) THEN
             DO I=1, 5
               QUANTITY(I) = 0
               SUBTOTAL(I) = 0
             END DO
          ELSE
            EXIT
          END IF

    ! exits and shuts down the program
         CASE(4)
           STOP

         CASE DEFAULT
            STOP
         
         END SELECT
         
         END DO

      END PROGRAM
