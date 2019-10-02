PROGRAM Fibonacci_memoization
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: min = 30000, max = 45000
  INTEGER :: i
  INTEGER, DIMENSION(:), ALLOCATABLE :: fibs

  ALLOCATE(fibs(max))
  fibs = -1
  
  DO i = min,max
     PRINT*, i, "-->", fibonacci(i)
  END DO
  
CONTAINS
  
  RECURSIVE FUNCTION FIBONACCI(N) RESULT (FIBO_RESULT)
    INTEGER, INTENT(IN)      :: N
    INTEGER                  :: FIBO_RESULT, fibs_1, fibs_2
    IF ( N <= 2 ) THEN
       FIBO_RESULT = 1
       fibs(n) = 1
    ELSE
       if (fibs(n-1) .ne. -1) then
          fibs_1 = fibs(n-1)
       else
          fibs_1 = fibonacci(N-1)
          fibs(n-1) = fibs_1
       end if

       if (fibs(n-2) .ne. -1) then
          fibs_2 = fibs(n-2)
       else
          fibs_2 = fibonacci(N-2)
          fibs(n-2) = fibs_2
       end if

       FIBO_RESULT = fibs_1 + fibs_2
    END IF
  END FUNCTION FIBONACCI
  
END PROGRAM Fibonacci_memoization
