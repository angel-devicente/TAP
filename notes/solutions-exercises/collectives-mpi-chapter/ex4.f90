PROGRAM main
  USE MPI
  IMPLICIT NONE

  INTEGER, parameter :: N=20, SEEDSIZE=50
  INTEGER  :: ierr, i, my_id, num_procs, seed, clock,id
  INTEGER, dimension(N)::array
  INTEGER, dimension(N)::array_final_sum
  INTEGER, dimension(N)::array_final_mult
  REAL, dimension(:), ALLOCATABLE :: array_rands
  REAL  :: r_num, max_value
  INTEGER :: sizer = 1
  INTEGER, dimension(SEEDSIZE) :: seedr

  DOUBLE PRECISION :: t0,t1,time

  CALL MPI_INIT( ierr )
  CALL MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr )
  CALL MPI_Comm_size ( MPI_COMM_WORLD, num_procs, ierr )

  t0 = MPI_WTIME()

  DO i=1,N
     array(i)= my_id+1
  END DO

  ! Sum
  CALL MPI_REDUCE(array, array_final_sum, N, MPI_INTEGER, MPI_SUM, 0 ,MPI_COMM_WORLD, ierr)
  IF( my_id .eq. 0) THEN
     WRITE(*,*) " Final array after sum ", array_final_sum(:)
  END IF
  
  ! Product
  CALL MPI_REDUCE(array, array_final_mult, N, MPI_INTEGER, MPI_PROD,0 ,MPI_COMM_WORLD, ierr)
  IF( my_id .eq. 0) THEN
     WRITE(*,*) " Final array after product: ", array_final_mult(:)
  END IF
  
  ! Random number generation
  CALL RANDOM_SEED(sizer)

#ifdef DEBUG
  seedr = my_id+1 
#else
  CALL SYSTEM_CLOCK(COUNT=clock)
  seedr = clock + 37 * (my_id+1) * (/ (i - 1, i = 1, SEEDSIZE) /)
#endif
  
  CALL RANDOM_SEED(put=seedr)
  CALL RANDOM_NUMBER(r_num)

  IF (my_id .eq. 0) ALLOCATE(array_rands(num_procs))
  CALL MPI_GATHER(r_num, 1, MPI_REAL, array_rands, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
  IF (my_id .eq. 0) THEN
     DO id=1,num_procs
        WRITE(*,*) "my_id", id-1, "  Random number :", array_rands(id)
     END DO
  END IF
  
  ! Search for the maximum value among generated random numbers...
  CALL MPI_REDUCE(r_num, max_value, 1, MPI_REAL, MPI_MAX, 0  ,MPI_COMM_WORLD, ierr)
  
  t1 = MPI_WTIME()
  time = t1 - t0

  IF( my_id .eq. 0) THEN
     WRITE(*,*) " Maximum generated random number :", max_value
     WRITE(*,*) " Total elapsed time [sec] : ", time
  END IF

  CALL MPI_FINALIZE(ierr)

END PROGRAM main
 
