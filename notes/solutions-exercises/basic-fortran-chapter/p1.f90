PROGRAM ONE
  IMPLICIT NONE

  INTEGER :: total = 0, limit = 1000
  INTEGER :: i

  DO i = 3,limit-1,3
     total = total + i
  END DO

  DO i = 5,limit-1,5
     IF (MOD(i,3) .NE. 0) total = total + i
  END DO
  
  PRINT*, total

END PROGRAM ONE
  
