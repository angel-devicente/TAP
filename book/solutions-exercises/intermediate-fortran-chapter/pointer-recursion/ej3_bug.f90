PROGRAM listas
  IMPLICIT NONE

  TYPE CELL
     INTEGER :: val
     TYPE (CELL), POINTER :: left,right
  END TYPE CELL

  TYPE (CELL), POINTER :: head
  INTEGER              :: n,k,i

  READ*, n

  READ*, k
  ALLOCATE(head)
  head%val = k
  NULLIFY(head%left)
  NULLIFY(head%right)

  DO i=2,n
     READ*, k
     CALL place_number(head,k)
  END DO

  CALL Print(head)

CONTAINS 
  RECURSIVE SUBROUTINE place_number(node,number)
    TYPE (CELL), POINTER :: node, temp
    INTEGER :: number

    IF (number < node%val) THEN
       IF (ASSOCIATED(node%left)) THEN
          CALL place_number(node%left,number)
       ELSE
          ALLOCATE(temp)
          node%left => temp
          temp%val = number
          NULLIFY(temp%left)
          NULLIFY(temp%right)
       END IF
    ELSE IF (number > node%val) THEN
       IF (ASSOCIATED(node%right)) THEN
          CALL place_number(node%right,number)
       ELSE
          ALLOCATE(temp)
          node%left => temp
          temp%val = number
          NULLIFY(temp%left)
          NULLIFY(temp%right)
       END IF
    ELSE
       PRINT*, "No se admiten numeros repetidos, ignorando el numero ",number, " !"
    END IF
  END SUBROUTINE place_number
  
  RECURSIVE SUBROUTINE Print (node) 
    TYPE (CELL), POINTER :: node

    IF (ASSOCIATED(node%left)) CALL Print (node%left)
    PRINT*, node%val
    IF (ASSOCIATED(node%right)) CALL Print (node%right)
    
  END SUBROUTINE Print
  
END PROGRAM listas
