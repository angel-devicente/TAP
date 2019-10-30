PROGRAM heat2D
  USE mpi
  IMPLICIT none

  INTEGER :: procs, rank, error
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status

  INTEGER :: lado, i, j, t=0
  INTEGER :: my_x, my_y, my_si, my_sj, my_ei, my_ej, my_lado, contact
  REAL :: my_min, my_max, my_diff

  REAL :: min,max,diff
  REAL, DIMENSION(:,:), ALLOCATABLE :: data
  REAL, DIMENSION(:), ALLOCATABLE :: data_temp

  CALL MPI_Init ( error )
  CALL MPI_Comm_size ( MPI_COMM_WORLD, procs, error )
  CALL MPI_Comm_rank ( MPI_COMM_WORLD, rank, error )

  IF (rank .EQ. 0) THEN
     READ*, lado
     ALLOCATE(data(0:lado+1,0:lado+1))

     data = 0.0

     DO i=1,lado
        READ*, data(i,1:lado)
     END DO

     CALL MPI_BCAST(lado,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(data,(lado+2)*(lado+2),MPI_REAL,0,MPI_COMM_WORLD,error)

  ELSE
     CALL MPI_BCAST(lado,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
     ALLOCATE(data(0:lado+1,0:lado+1))
     CALL MPI_BCAST(data,(lado+2)*(lado+2),MPI_REAL,0,MPI_COMM_WORLD,error)
  END IF

  my_lado = lado / 2
  ALLOCATE(data_temp(my_lado))

  my_x = (rank / 2)
  my_y = MOD(rank,2)

  my_si = my_x*my_lado + 1
  my_sj = my_y*my_lado + 1

  my_ei = my_si + my_lado - 1
  my_ej = my_sj + my_lado - 1

  my_min=MINVAL(data(my_si:my_ei,my_sj:my_ej))
  my_max=MAXVAL(data(my_si:my_ei,my_sj:my_ej))

  CALL MPI_ALLREDUCE(my_min,min,1,MPI_REAL,MPI_MIN,MPI_COMM_WORLD,error)
  CALL MPI_ALLREDUCE(my_max,max,1,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,error)

  diff = max - min
  IF (rank .EQ. 0)   PRINT*, "t = ",t, "diff = ",diff

  DO WHILE (diff .GE. 1)

     t=t+1
     data(my_si:my_ei,my_sj:my_ej) = 0.99*data(my_si:my_ei,my_sj:my_ej) + 0.01*((data(my_si-1:my_ei-1,my_sj:my_ej) + &
          data(my_si+1:my_ei+1,my_sj:my_ej) + data(my_si:my_ei,my_sj-1:my_ej-1) + data(my_si:my_ei,my_sj+1:my_ej+1)) / 4)


     !! Envio de datos

     !! Envio de filas (TAG = 10)
     !! Si my_x = 0, tengo que enviar mi ultima fila a my_y + 2
     !! Si my_x <> 0, tengo que enviar mi primera fila a my_y

     IF (my_x .EQ. 0) THEN
        contact = my_y + 2
        data_temp = data(my_ei,my_sj:my_ej)
        CALL MPI_Send(data_temp,my_lado,MPI_REAL,contact,10,MPI_COMM_WORLD,error)
        CALL MPI_Recv(data_temp,my_lado,MPI_REAL,contact,10,MPI_COMM_WORLD,status,error)
        data(my_ei+1,my_sj:my_ej) = data_temp
     ELSE
        contact = my_y
        CALL MPI_Recv(data_temp,my_lado,MPI_REAL,contact,10,MPI_COMM_WORLD,status,error)
        data(my_si-1,my_sj:my_ej) = data_temp
        data_temp = data(my_si,my_sj:my_ej)
        CALL MPI_Send(data_temp,my_lado,MPI_REAL,contact,10,MPI_COMM_WORLD,error)
     END IF

     !! Envio de columnas (TAG = 20)
     !! Si my_y = 0, tengo que enviar mi ultima columna a my_x*2 + 1
     !! Si my_y <> 0, tengo que enviar mi primera columna a my_x*2

     IF (my_y .EQ. 0) THEN
        contact = (my_x * 2) + 1
        data_temp = data(my_si:my_ei,my_ej)
        CALL MPI_Send(data_temp,my_lado,MPI_REAL,contact,20,MPI_COMM_WORLD,error)
        CALL MPI_Recv(data_temp,my_lado,MPI_REAL,contact,20,MPI_COMM_WORLD,status,error)
        data(my_si:my_ei,my_ej+1) = data_temp
     ELSE
        contact = my_x * 2
        CALL MPI_Recv(data_temp,my_lado,MPI_REAL,contact,20,MPI_COMM_WORLD,status,error)
        data(my_si:my_ei,my_sj-1) = data_temp
        data_temp = data(my_si:my_ei,my_sj)
        CALL MPI_Send(data_temp,my_lado,MPI_REAL,contact,20,MPI_COMM_WORLD,error)
     END IF

     my_min=MINVAL(data(my_si:my_ei,my_sj:my_ej))
     my_max=MAXVAL(data(my_si:my_ei,my_sj:my_ej))

     CALL MPI_ALLREDUCE(my_min,min,1,MPI_REAL,MPI_MIN,MPI_COMM_WORLD,error)
     CALL MPI_ALLREDUCE(my_max,max,1,MPI_REAL,MPI_MAX,MPI_COMM_WORLD,error)

     diff = max - min
     IF (rank .EQ. 0)   THEN
        IF (MOD(t,1000) .EQ. 0) PRINT*, "t = ",t, "diff = ",diff
     END IF

  END DO

  IF (rank .EQ. 0) THEN
     PRINT*, "Final t is: ", t
  END IF

  CALL MPI_Finalize ( error )
END PROGRAM heat2D
