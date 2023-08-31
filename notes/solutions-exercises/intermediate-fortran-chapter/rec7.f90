PROGRAM CON_DIGITS
  IMPLICIT NONE

  INTEGER :: n1,n2

  DO
     READ*, n1,n2
     IF (n1 .EQ. 0) EXIT
     PRINT*, contained_digits(n1,n2)
  END DO
  
CONTAINS

  RECURSIVE FUNCTION contained_digits(n1,n2) RESULT(res)
    INTEGER :: n1,n2,n2t,rem
    LOGICAL :: res
    
    n2t = n2
    res = .FALSE.
    
    IF (n1 / 10 < 1) THEN
       ! Just one digit. No need for recursion
       DO WHILE (n2t > 0)
          rem = MOD(n2t,10)
          n2t = n2t / 10
          IF (n1 == rem) THEN
             res = .TRUE.
             EXIT
          END IF
       END DO
    ELSE
       IF (contained_digits(MOD(n1,10),n2)) THEN
          res = contained_digits(n1/10,n2)
       END IF
    END IF
  END FUNCTION contained_digits
  
END PROGRAM CON_DIGITS
