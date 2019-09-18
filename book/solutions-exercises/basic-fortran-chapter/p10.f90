PROGRAM EULER
  IMPLICIT NONE

  INTEGER, PARAMETER :: int16 = selected_int_kind(16)
  INTEGER, PARAMETER :: primes_max = 2000000
  LOGICAL, DIMENSION(primes_max) :: primes

  INTEGER :: imax,i,j
  INTEGER (kind=int16) :: numb

  primes = .TRUE.
  primes(1) = .FALSE.

  imax=SQRT(REAL(primes_max))

  DO i=2,imax
     DO j=2*i,primes_max,i
        primes(j)=.FALSE.
     END DO
  END DO

  numb = 0
  DO i=1,primes_max-1
     IF (primes(i)) numb = numb + i
  END DO
  
  PRINT*, numb
END PROGRAM EULER
