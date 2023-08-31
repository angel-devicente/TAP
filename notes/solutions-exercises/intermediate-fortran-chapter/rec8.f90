PROGRAM PERMUTATIONS
  IMPLICIT NONE

  INTEGER :: n,naux
  INTEGER :: dn
  INTEGER, DIMENSION(:),ALLOCATABLE :: an,perm
  LOGICAL, DIMENSION(:), ALLOCATABLE :: used

  READ*,n

  naux=n
  dn = 0
  DO WHILE(naux > 0)
     dn = dn + 1
     naux = naux / 10
  END DO

  ALLOCATE(an(dn))

  naux=n
  dn=0
  DO WHILE(naux > 0)
     dn = dn + 1
     an(dn) = MOD(naux,10)
     naux = naux / 10
  END DO
  an(dn:1:-1) = an(1:dn)

  ALLOCATE(used(dn),perm(dn))
  used = .FALSE.

  CALL PERMS(1)
  
CONTAINS

  RECURSIVE SUBROUTINE PERMS(level)
    INTEGER :: level,i

    IF (level .EQ. dn) THEN
       ! Base case. If here, everytime we add a new number to the last position in perm
       !  we have a new permutation and have to print it.
       DO i=1,dn
          IF (.NOT. used(i)) THEN
             perm(level) = an(i)
             PRINT*,perm
          END IF
       END DO
    ELSE
       DO i=1,dn
          IF (.NOT. used(i)) THEN
             perm(level) = an(i)
             used(i) = .TRUE.
             CALL PERMS(level+1)
             used(i) = .FALSE.
          END IF
       END DO
    END IF

  END SUBROUTINE PERMS
  
END PROGRAM PERMUTATIONS
