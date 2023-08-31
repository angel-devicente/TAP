PROGRAM mpi2
  USE mpi
  IMPLICIT none

  INTEGER :: procs, rank, error, send_to
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

  INTEGER :: number, number_p, total, partial, start
  INTEGER, DIMENSION( : ), ALLOCATABLE :: data, data_p

  CALL MPI_Init ( error )
  CALL MPI_Comm_size ( MPI_COMM_WORLD, procs, error )
  CALL MPI_Comm_rank ( MPI_COMM_WORLD, rank, error )



  IF ( rank .EQ. 0 ) THEN
     READ*, number
     ALLOCATE (data(number))
     READ*, data

     number_p = number / procs
     total = SUM(data(1:number_p))

     DO send_to = 1, procs - 1
        start = (number_p * send_to) + 1
        CALL MPI_Send(number_p,1,MPI_INTEGER,send_to,1,MPI_COMM_WORLD,error)
        CALL MPI_Send(data(start),number_p,MPI_INTEGER,send_to,0,MPI_COMM_WORLD,error)
     END DO

     DO send_to = 1, procs - 1
        CALL MPI_Recv(partial,1,MPI_INTEGER,MPI_ANY_SOURCE,0,MPI_COMM_WORLD,status,error)
        total = total + partial
     END DO

     PRINT*, "The total is ", total
  ELSE
     CALL MPI_Recv(number_p,1,MPI_INTEGER,0,1,MPI_COMM_WORLD,status,error)
     ALLOCATE (data_p(number_p))
     CALL MPI_Recv(data_p,number_p,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,error)
     partial = SUM(data_p)
     CALL MPI_Send(partial,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,error)
  END IF

  CALL MPI_Finalize ( error )

END PROGRAM mpi2
