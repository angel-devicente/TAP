PROGRAM pointerswap
  IMPLICIT NONE

  INTEGER, TARGET :: s1,s2
  INTEGER, POINTER :: p1,p2

  s1=4 ; s2=6
  p1=>s1 ; p2=>s2

  CALL swap_with_pointers(p1,p2)

  PRINT*, "Via pointers:" ,p1,p2
  PRINT*, "Via variables:",s1,s2

CONTAINS

  SUBROUTINE swap_with_pointers(pt1,pt2)
    INTEGER, INTENT(IN), POINTER :: pt1,pt2
    INTEGER :: swap

    swap = pt1
    pt1 = pt2
    pt2 = swap

  END SUBROUTINE swap_with_pointers
    
END PROGRAM pointerswap
