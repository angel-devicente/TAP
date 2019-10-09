PROGRAM listas
  IMPLICIT NONE

  TYPE CELL
     INTEGER :: val
     TYPE (CELL), POINTER :: prev
     TYPE (CELL), POINTER :: next
  END TYPE CELL

  TYPE (CELL), TARGET  :: head
  TYPE (CELL), POINTER :: curr, temp
  INTEGER              :: n,k,i

  head%val = 0
  NULLIFY(head%prev)
  NULLIFY(head%next)

  PRINT*, "Cuantos valores?"
  READ*, n

  DO i=1,n
     READ*, k
     ALLOCATE(temp)
     temp%val = k
     NULLIFY(temp%prev)
     NULLIFY(temp%next)

     curr => head
     DO 
        IF (ASSOCIATED(curr%next)) THEN
           IF (curr%next%val .GE. k) THEN
              EXIT
           ELSE
              curr => curr%next
           END IF
        ELSE
           EXIT
        END IF
     END DO

     IF (ASSOCIATED(curr%next)) THEN
        curr%next%prev => temp
     END IF
     temp%next => curr%next
     temp%prev => curr
     curr%next => temp

  END DO

PRINT*, " Backwards..."
curr => head
DO
   IF (.NOT. ASSOCIATED(curr%next)) EXIT
   curr => curr%next
END DO
CALL Print (curr,0)

PRINT*, " Forward..."
curr => head
CALL Print (curr,1)

CONTAINS 
  RECURSIVE SUBROUTINE Print (ptr,forward) 
    TYPE (CELL), POINTER :: ptr
    INTEGER :: forward

    PRINT*, ptr%val
    IF (forward == 1 .AND. ASSOCIATED(ptr%next)) CALL Print (ptr%next,1)
    IF (forward == 0 .AND. ASSOCIATED(ptr%prev)) CALL Print (ptr%prev,0)
    
  END SUBROUTINE Print
  
END PROGRAM listas
