PROGRAM TSP
  IMPLICIT NONE

  INTEGER :: n,i,j,k,l,m
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: distances
  INTEGER, DIMENSION(:), ALLOCATABLE :: route
  INTEGER :: dist_travelled, min_dist = HUGE(0)
  
  READ*, n
  IF (n .NE. 5) STOP "This version only works with n == 5"
  ALLOCATE(distances(n,n))
  ALLOCATE(route(n))

  DO i=1,n
     READ*, distances(i,:)
  END DO

  LI: DO I = 1, n
     LJ: DO J = 1, n
        IF (J==I) CYCLE
        LK: DO K = 1, n
           IF (K==J .OR. K==I) CYCLE
           LL: DO L = 1, n
              IF (L==J .OR. L==K .OR. L==I ) CYCLE
              LM: DO M = 1, n
                 IF (M==L .OR. M==K .OR. M==J .OR. M==I) CYCLE
                 dist_travelled = distances(I,J) + distances(J,K) + distances(K,L) + &
                      distances(L,M) + distances(M,I)
                 IF (dist_travelled < min_dist) THEN
                    min_dist = dist_travelled
                    route(1) = I
                    route(2) = J
                    route(3) = K
                    route(4) = L
                    route(5) = M
                 END IF
              END DO LM
           END DO LL
        END DO LK
     END DO LJ
  END DO LI
  PRINT*,'Shortest route travelled is :', route
  PRINT*,'Distance travelled = ',min_dist
  
END PROGRAM TSP
