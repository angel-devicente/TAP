PROGRAM mpi1
  USE mpi
  IMPLICIT none

  INTEGER :: procs, rank, error, number, send_to
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

  CALL MPI_Init ( error )
  CALL MPI_Comm_size ( MPI_COMM_WORLD, procs, error )
  CALL MPI_Comm_rank ( MPI_COMM_WORLD, rank, error )

  IF ( rank .EQ. procs - 1) THEN
     send_to = 0
  ELSE
     send_to = rank + 1
  END IF

  IF ( rank .EQ. 0 ) THEN
     READ*, number
     CALL MPI_Send(number,1,MPI_INTEGER,send_to,0,MPI_COMM_WORLD,error)
     CALL MPI_Recv(number,1,MPI_INTEGER,MPI_ANY_SOURCE,0,MPI_COMM_WORLD,status,error)
     PRINT*, number
  ELSE
     CALL MPI_Recv(number,1,MPI_INTEGER,MPI_ANY_SOURCE,0,MPI_COMM_WORLD,status,error)
     number = number * (rank + 1)
     CALL MPI_Send(number,1,MPI_INTEGER,send_to,0,MPI_COMM_WORLD,error)
  END IF

  CALL MPI_Finalize ( error )

END PROGRAM mpi1
