PROGRAM FOUR
  IMPLICIT NONE

  INTEGER :: number,n1,n2,n2min,pos,solution=0
  INTEGER, DIMENSION(6) :: digits

  n2min = 100
  OUT:  DO n1 = 999,100,-1
     IF (n1**2 .LE. solution) EXIT
     DO n2 = n1,n2min,-1
        number = n1 * n2
        IF (number .LE. solution .OR. number .LT. 100000) THEN
           CYCLE OUT
        ELSE
           DO pos=1,6
              digits(pos) = MOD(number,10)
              number = number / 10
           END DO
           IF (digits(1) .EQ. digits(6) .AND. &
                digits(2) .EQ. digits(5) .AND. &
                digits(3) .EQ. digits(4)) THEN
              IF (n1*n2 .GT. solution) solution = n1*n2
              n2min = n2
              CYCLE OUT
           END IF
        END IF
     END DO
  END DO OUT

  PRINT*, solution

END PROGRAM FOUR
