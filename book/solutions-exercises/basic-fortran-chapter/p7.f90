PROGRAM EULER
  IMPLICIT NONE
  INTEGER, PARAMETER :: prime_max = 150000
  LOGICAL, DIMENSION(prime_max) :: primes
  INTEGER :: imax,i,j,numb

  primes = .TRUE.
  primes(1) = .FALSE.

  imax=SQRT(REAL(prime_max))

  DO i=2,imax
     DO j=2*i,prime_max,i
        primes(j)=.FALSE.
     END DO
  END DO

  numb = 0
  DO i=1,prime_max
     IF  (primes(i)) THEN
        numb=numb+1
        IF (numb .EQ. 10001) THEN
           PRINT*, i
           EXIT
        END IF
     END IF
  END DO

  PRINT*, numb
  
END PROGRAM EULER
