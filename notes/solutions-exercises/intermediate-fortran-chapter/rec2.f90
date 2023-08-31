PROGRAM Fibonacci_iterative
  IMPLICIT NONE

  INTEGER, PARAMETER :: min = 30000, max = 45000
  INTEGER :: i
  
  DO i = min,max
     PRINT*, i, "-->", fibonacci(i)
  END DO
  
CONTAINS
  
  INTEGER FUNCTION FIBONACCI(N) 
    INTEGER, INTENT(IN)      :: N
    INTEGER :: a,b,t,i
    
    a=1 ; b=1 

    if ( N <= 2 ) THEN
       fibonacci = 1
    else
       do i=3,n
          t=a
          a=b
          b=b+t
       end do
       fibonacci = b
    end if
  end FUNCTION FIBONACCI
  
END PROGRAM Fibonacci_iterative
