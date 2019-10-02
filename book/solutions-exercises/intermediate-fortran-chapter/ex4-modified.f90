PROGRAM pascal_triangle
  IMPLICIT NONE
  
  INTEGER :: num_rows 
  INTEGER :: ii
  INTEGER, DIMENSION(:), ALLOCATABLE :: row, prev_row  

  READ*, num_rows

  ALLOCATE(row(num_rows),prev_row(num_rows))

  CALL p_row(num_rows,row,prev_row)
  PRINT*, row

  DEALLOCATE(row,prev_row)

CONTAINS

  RECURSIVE SUBROUTINE p_row(nn,row,prev_row)
    INTEGER, INTENT(IN) :: nn
    INTEGER, DIMENSION(:), ALLOCATABLE :: row, prev_row  
    INTEGER :: jj 
  
    IF(nn .EQ. 1) THEN
      row(:nn)=1
    ELSE
      row(1)  = 1
      row(nn) = 1 
      CALL p_row(nn-1,prev_row,row)
      PRINT*, prev_row(:nn-1)

      DO jj=2, nn-1, 1
        row(jj) =  prev_row(jj-1)+prev_row(jj)
      END DO
    END IF

  END SUBROUTINE p_row

END PROGRAM pascal_triangle
