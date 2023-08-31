PROGRAM E6
  IMPLICIT NONE
  
  INTEGER :: size_p
  INTEGER, DIMENSION(:), ALLOCATABLE :: line_a,line_b

  DO size_p = 1,6
     PRINT*, "PASCAL TRIANGLE for n =", size_p
     ALLOCATE(line_a(size_p),line_b(size_p))
     line_a(1)=1
     CALL generate_pascal_triangle(1,line_a,line_b)
     DEALLOCATE(line_a,line_b)
     PRINT*, ""
  END DO
  
CONTAINS
  
  RECURSIVE SUBROUTINE generate_pascal_triangle(n,line_prev,line_next)
    INTEGER, INTENT(IN)      :: n
    INTEGER, DIMENSION(:)    :: line_prev, line_next
    INTEGER :: i

    IF (n > size_p) THEN
       RETURN
    ELSE 
       line_next(1) = 1
       IF (n > 1) THEN
          line_next(n) = 1
          DO i=2,n-1
             line_next(i) = line_prev(i-1) + line_prev(i)
          END DO
       END IF
    END IF

    PRINT*, line_next(:n)
    CALL generate_pascal_triangle(n+1,line_next,line_prev)

  END SUBROUTINE generate_pascal_triangle

  
END PROGRAM E6
