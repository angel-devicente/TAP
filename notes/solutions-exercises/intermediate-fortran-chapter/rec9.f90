PROGRAM QUEENS
  IMPLICIT NONE

  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: board
  INTEGER :: n,solutions
  
  PRINT*, "Board size (side)"
  READ*,n

  ALLOCATE(board(n,n))
  board = .FALSE.   ! .FALSE. = empty cell, .TRUE. = a cell with a queen
  solutions = 0
  
  CALL add_queen(1,n)
  PRINT*, "Number of solutions (not taking into account rotation and reflection): ", solutions
  
CONTAINS

  RECURSIVE SUBROUTINE add_queen(row,n)
    INTEGER, INTENT(IN) :: row,n
    INTEGER :: col

    IF (row .EQ. n) THEN
       ! Base case. If we are able to place a queen in this row, then print board

       DO col=1,n
          board(row,col) = .TRUE.
          IF (valid_board(row,col,n)) THEN
             CALL print_board(n)
             solutions = solutions + 1
          END IF
          board(row,col) = .FALSE.
       END DO
    ELSE
       DO col=1,n
          board(row,col) = .TRUE.
          IF (valid_board(row,col,n)) THEN
             CALL add_queen(row+1,n)
          END IF
          board(row,col) = .FALSE.
       END DO
    END IF
  END SUBROUTINE add_queen

  SUBROUTINE print_board(n)
    INTEGER :: n,row

    PRINT*,""
    PRINT*, "==================="
    DO row=1,n
       PRINT*, board(row,:)
    END DO
  END SUBROUTINE print_board
  
  ! This function will just make sure that if we add a queen in [row,col]
  !  this new queen doesn't attack any previously located queens (i.e. those
  !  positions in the board == .TRUE.
  FUNCTION valid_board(row,col,n)
    LOGICAL :: valid_board
    INTEGER :: row,col,n,ri,ci,count,diff

    valid_board = .TRUE.

    ! Horizontally
    count = 0
    DO ci=1,n
       IF (board(row,ci)) count = count + 1
    END DO
    IF (count > 1) THEN
       valid_board = .FALSE.
       RETURN
    END IF

    ! Vertically
    count = 0
    DO ri=1,n
       IF (board(ri,col)) count = count + 1
    END DO
    IF (count > 1) THEN
       valid_board = .FALSE.
       RETURN
    END IF

    ! SE diagonal
    count = 0
    diff = MIN(row,col) - 1
    ri = row-diff ; ci = col-diff
    DO
       IF (ri > n .OR. ci > n) EXIT
       IF (board(ri,ci)) count = count + 1
       ri = ri + 1 ; ci = ci + 1
    END DO
    IF (count > 1) THEN
       valid_board = .FALSE.
       RETURN
    END IF

    ! NE diagonal
    count = 0
    diff = MIN(n-row,col-1)
    ri = row+diff ; ci = col-diff
    DO
       IF (ri < 1 .OR. ci > n) EXIT
       IF (board(ri,ci)) count = count + 1
       ri = ri - 1 ; ci = ci + 1
    END DO
    IF (count > 1) THEN
       valid_board = .FALSE.
       RETURN
    END IF

    
  END FUNCTION valid_board
  
END PROGRAM QUEENS
