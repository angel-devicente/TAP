PROGRAM FIVE
  IMPLICIT NONE
  
  INTEGER :: number,pos
  INTEGER, DIMENSION(8) :: divisors
  
  divisors = (/11, 12, 13, 14, 15, 16, 17, 18/)
  
  number = 0
  
  OUT:  DO
     number = number + 380
     DO pos = 8,1,-1
        IF (MOD(number,divisors(pos)) .NE. 0) CYCLE OUT
     END DO
     EXIT
  END DO OUT
  
  PRINT*, number
  
END PROGRAM FIVE
