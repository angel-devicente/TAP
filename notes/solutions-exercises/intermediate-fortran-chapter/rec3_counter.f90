PROGRAM Fibo_counter
  IMPLICIT NONE

  INTEGER, PARAMETER :: min = 35, max = 40
  INTEGER :: i
  
  DO i = min,max
     PRINT*, i, "-->", fibonacci_c(i)
  END DO
  
CONTAINS
  
  RECURSIVE FUNCTION FIBONACCI_C(N) RESULT (FIBO_RESULT)
    INTEGER, INTENT(IN)      :: N
    INTEGER                  :: FIBO_RESULT
    IF ( N <= 2 ) THEN
       FIBO_RESULT = 1
    ELSE
       FIBO_RESULT = 1 + FIBONACCI_C(N-1) + FIBONACCI_C(N-2)
    END IF
  END FUNCTION FIBONACCI_C
  
END PROGRAM Fibo_Counter
