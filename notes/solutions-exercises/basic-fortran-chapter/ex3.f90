!! To verify: http://kinetigram.com/mck/LinearAlgebra/JPaisMatrixMult04/classes/JPaisMatrixMult04.html

PROGRAM matmul
  IMPLICIT NONE

  INTEGER :: m,n,p
  INTEGER :: i,j
  REAL, DIMENSION(:,:), ALLOCATABLE :: A,B,C


  READ*, m,n,p

  ALLOCATE(A(m,n))
  ALLOCATE(B(n,p))
  ALLOCATE(C(m,p))

  DO i=1,m
     READ*, A(i,:)
  END DO

  DO i=1,n
     READ*, B(i,:)
  END DO

  DO i=1,m
     DO j=1,p
        C(i,j) = SUM(A(i,:) * B(:,j))
     END DO
  END DO

  DO i=1,m
     PRINT*, C(i,:)
  END DO

END PROGRAM matmul
