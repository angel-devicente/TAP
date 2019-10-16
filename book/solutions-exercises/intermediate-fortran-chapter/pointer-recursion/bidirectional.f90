PROGRAM bidirectional
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
  curr => head

  PRINT*, "Input number of elements in the list"
  READ*, n

  PRINT*, "Now enter", n, " elements"
  DO i=1,n
     READ*, k
     ALLOCATE(temp)
     temp%val = k
     NULLIFY(temp%prev)
     NULLIFY(temp%next)
     curr%next => temp
     temp%prev => curr

     curr => temp
  END DO

PRINT*, " Backwards..."
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
  
END PROGRAM bidirectional
