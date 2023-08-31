PROGRAM SUMMATION
  IMPLICIT NONE

  INTEGER :: n,w,i, sumi
  REAL, DIMENSION(:), ALLOCATABLE :: data
  REAL :: bsum = 0, psum

  READ*, n
  READ*, w

  ALLOCATE(data(n))
  DO i=1,n
     READ*, data(i)
  END DO

  DO i=w,n
     psum = SUM(data(i-w+1:i))
     IF (psum > bsum) THEN
        bsum = psum
        sumi = i
     END IF
  END DO

  PRINT*, "Greatest sum is:", bsum, "given by the following numbers:"
  DO i=sumi-w+1,sumi
     PRINT*, data(i)
  END DO
  
END PROGRAM SUMMATION
