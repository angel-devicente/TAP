PROGRAM main
  USE MPI
  IMPLICIT NONE

  INTEGER, parameter::N=50
  INTEGER ::my_id, num_procs, ierr
  INTEGER ::i, num_elem
  INTEGER, dimension(N)::array, array_final
  INTEGER, allocatable, dimension(:)::array_recv
  INTEGER, allocatable:: sendcount(:), displs(:)

  CALL MPI_INIT( ierr )
  CALL MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr )
  CALL MPI_Comm_size ( MPI_COMM_WORLD, num_procs, ierr )

  ! proc 0 initializes the principal array
  IF( my_id .eq. 0) THEN
     DO i=1,N
        array(i)= i
     END DO
  END IF

  num_elem= N/num_procs
  IF (my_id < MOD(N,num_procs)) THEN
     num_elem = num_elem +1
  ENDIF

  ALLOCATE(array_recv(num_elem))

  ! in case that N is a multiple of the number of MPI tasks, the same number of
  ! elements is send to (and received from) by root process to others, so
  ! mpi_scatter and mpi_gather are called
  IF ( MOD(N,num_procs) .eq. 0 ) THEN
     CALL MPI_SCATTER(array, num_elem, MPI_INTEGER, array_recv, num_elem, &
          MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     WRITE(*,*) "my_id", my_id, "elementi ricevuti:", array_recv(1:num_elem)

     DO i=1,num_elem
        array_recv(i)= array_recv(i)+my_id
     END DO

     CALL MPI_GATHER(array_recv, num_elem, MPI_INTEGER, array_final, &
          num_elem, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     IF( my_id .eq. 0) THEN
        WRITE(*,*) "N is multiple of num_procs, mod(N,num_procs)= ", &
             mod(N,num_procs)
        WRITE(*,*) " array finale: ", array_final(:)
     END IF

  ELSE

     !in case that N is not a multiple of the number of MPI tasks,
     !mpi_scatterv and mpi_gatherv have to be used

     ALLOCATE(sendcount(num_procs),displs(num_procs))

     displs(1) = 0
     sendcount=N/num_procs
     if(0<mod(N,num_procs)) sendcount(1)=N/num_procs+1

     DO i=2,num_procs
        if( (i-1) < mod(N,num_procs) ) sendcount(i) = N/num_procs +1
        displs(i) = SUM(sendcount(1:i-1))
     END DO

     IF (my_id .eq. 0 ) THEN
        WRITE(*,*) "sendcount: ", sendcount
        WRITE(*,*) "displs: ", displs
     END IF

     CALL MPI_SCATTERV(array, sendcount, displs, MPI_INTEGER, &
          array_recv, num_elem, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     WRITE(*,*) "my_id", my_id, "elementi ricevuti:", array_recv(1:num_elem)

     DO i=1,num_elem
        array_recv(i)= array_recv(i)+my_id
     END DO

     CALL MPI_GATHERV(array_recv, num_elem, MPI_INTEGER, array_final, &
          sendcount, displs, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

     IF( my_id .eq. 0) THEN
        WRITE(*,*) "N is not multiple of num_procs, mod(N,num_procs)= ", &
             mod(N,num_procs)
        WRITE(*,*) " array finale: ", array_final(:)
     END IF
  ENDIF

  DEALLOCATE(array_recv)
  CALL MPI_FINALIZE(ierr)
END PROGRAM main
