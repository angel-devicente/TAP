PROGRAM TWO
  IMPLICIT NONE

  INTEGER, POINTER :: first,second,third
  INTEGER :: total

  ALLOCATE(first,second,third)

  first = 1
  second = 2
  third = 3

  total = 2

  DO 
     first => second
     second => third
     third => first
     third = first + second

     IF (third .GT. 4000000) THEN
        EXIT
     ELSEIF (MOD(third,2) .EQ. 0) THEN
        total = total + third
     END IF
  END DO

  PRINT*, total
END PROGRAM TWO
