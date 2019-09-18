PROGRAM EULER
  IMPLICIT NONE

  INTEGER :: a,b,c
  INTEGER, PARAMETER :: sumpyth = 1000
  
  INA: DO a = 1,sumpyth/3
     INB: DO b=a+1,sumpyth/2
        c = sumpyth - (a+b)
        IF (c < b) CYCLE INA
        IF (a**2 + b**2 .EQ. c**2) THEN
           PRINT*, a,b,c,a*b*c
           EXIT INA
        END IF
     END DO INB
  END DO INA
END PROGRAM EULER
