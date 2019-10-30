PROGRAM RT_LIKE
  IMPLICIT NONE

  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: atmos
  INTEGER :: nx,ny
  
  READ*, nx,ny
  ALLOCATE(atmos(nx,ny))

  !! Initialize array with some sample data
  CALL init_atmos()

  PRINT*, " ------------ Initial values -----------------"
  CALL print_atmos()

  PRINT*, ""
  CALL rt_l()
  PRINT*, ""
  
  PRINT*, " ------------ Final values -----------------"
  CALL print_atmos()
  
CONTAINS

  ! --------------------------------------------
  ! rt_l
  !
  ! This routine is fixed for 45º angles
  ! --------------------------------------------
  SUBROUTINE rt_l()
    INTEGER :: st_row,st_col,uw_row,uw_col,dw_row,dw_col

    ! Rays starting in column 1
    DO st_row=1,nx
       uw_row = st_row
       uw_col = 1
!       PRINT*, "Doing ray starting at:", uw_row,uw_col
       CALL propagate(uw_row,uw_col)
    END DO

    ! Rays starting in row nx
    DO st_col=2,ny
       uw_row = nx
       uw_col = st_col
!       PRINT*, "Doing ray starting at;", uw_row,uw_col
       CALL propagate(uw_row,uw_col)
    END DO
  END SUBROUTINE rt_l
  
  ! --------------------------------------------
  ! propagate
  !
  ! --------------------------------------------
  SUBROUTINE propagate(uw_row,uw_col)
    INTEGER :: uw_row,uw_col,dw_row,dw_col

    DO
       dw_row = uw_row - 1
       dw_col = uw_col + 1
       IF (dw_row < 1 .OR. dw_col > ny) EXIT
       atmos(dw_row,dw_col) = (atmos(uw_row,uw_col) + atmos(dw_row,dw_col)) * 0.5
!       PRINT*, "pos",dw_row,dw_col,atmos(uw_row,uw_col),atmos(dw_row,dw_col)
       uw_row = dw_row
       uw_col = dw_col
    END DO
  END SUBROUTINE propagate

  ! --------------------------------------------
  ! init_atmos
  !
  ! --------------------------------------------
  SUBROUTINE init_atmos()
    INTEGER :: x,y
    
    DO x=1,nx
       DO y=1,ny
          atmos(x,y) = 1.234 * x + 2.345 * y
       END DO
    END DO
  END SUBROUTINE init_atmos

  ! --------------------------------------------
  ! print_atmos
  !
  ! --------------------------------------------
  SUBROUTINE print_atmos()
    INTEGER :: x
    CHARACTER(LEN=50) :: fmt

    write(fmt,*) "(" , ny , "E10.3)"
    DO x=1,nx
       WRITE(*,fmt) atmos(x,:)
    END DO
  END SUBROUTINE print_atmos
  
END PROGRAM RT_LIKE
