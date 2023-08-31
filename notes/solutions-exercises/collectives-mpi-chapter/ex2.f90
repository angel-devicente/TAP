PROGRAM trapezoidal
  USE MPI
  IMPLICIT NONE

  INTEGER :: n, dest=0, tag=0
  REAL :: a, b
  INTEGER :: my_rank, p, local_n, source, status(MPI_STATUS_SIZE), ierr
  REAL :: h, local_a, local_b, integral, total
  
  CALL MPI_INIT(ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD, p, ierr)

  CALL Get_data (a, b, n, my_rank, p)

  h = (b-a)/n
  local_n = n/p

  local_a = a + my_rank*local_n*h
  local_b = local_a + local_n*h
  integral = Trap(local_a, local_b, local_n, h)

  CALL MPI_REDUCE(integral,total,1,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  
  IF (my_rank .EQ. 0) THEN
     PRINT*, 'With n=', n, 'trapezoids, our estimate'
     PRINT*, 'of the integral from', a, 'to', b, '=', total
  ENDIF

  CALL MPI_FINALIZE(ierr)

CONTAINS
  
  SUBROUTINE Get_data(a, b, n, my_rank, p)
    REAL :: a, b
    INTEGER :: n, my_rank, p, source=0, dest, tag, status(MPI_STATUS_SIZE), ierr
    
    IF (my_rank .EQ. 0) THEN
       PRINT*, "Enter a, b and n"
       READ*, a, b, n
    END IF
    
    CALL MPI_BCAST(a,1,MPI_REAL, 0,MPI_COMM_WORLD, ierr )
    CALL MPI_BCAST(b,1,MPI_REAL, 0,MPI_COMM_WORLD, ierr )
    CALL MPI_BCAST(n,1,MPI_REAL, 0,MPI_COMM_WORLD, ierr )
  END SUBROUTINE Get_data


  REAL FUNCTION f(x)
    REAL :: x
    
    f = x*x
  END FUNCTION f
  
  REAL FUNCTION Trap(local_a, local_b, local_n, h)
    REAL    :: local_a, local_b, h, integral, x
    INTEGER :: local_n,i
    
    integral = (f(local_a) + f(local_b))/2.0 
    x = local_a 
    DO i = 1, local_n-1
       x = x + h 
       integral = integral + f(x) 
    END DO
    Trap = integral*h 
  END FUNCTION Trap
  
END PROGRAM trapezoidal
