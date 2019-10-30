program distribuite

  use mpi

  implicit none

  character(LEN=50) :: stringa

  integer, parameter :: N = 10

  integer ierr, error
  integer status(MPI_STATUS_SIZE)

  integer k, i, j, rem, iglob, Ncol, Nrow, sup
  integer me, nprocs

  integer, allocatable, dimension(:,:) :: a

  call MPI_INIT(ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, me, ierr)

  !$ Number of rows that are assigned to each processor, taking care of the remainder
  Nrow = N
  Ncol = N / nprocs

  rem = MOD(N, nprocs)
  if (me < rem) then
     Ncol = Ncol + 1
  endif

  !$ Allocate local workspace (and notice that the array is distributed by columns)
  ALLOCATE( a(Nrow, Ncol) )
  !$ Logical column (Local row) of the first "one" entry
  iglob = (Ncol * me) + 1
  if (me >= rem) then
     iglob = iglob + rem
  endif
  !$ Initilize local matrix
  do j=1,Ncol
     do i=1,Nrow
        if (i == iglob) then
           A(i,j) = 1.0
        else
           A(i,j) = 0.0
        endif
     enddo
     iglob = iglob + 1;
  enddo

  write(stringa, *) Nrow
  !$ Print matrix
  if (me == 0) then
     !$ Rank 0: print local buffer
     do j=1,Ncol
        print '('//trim(stringa)//'(I2))', A(:,j)
     enddo
     !$ Receive new data from other processes
     !$ in an ordered fashion and print the buffer
     do k=1, nprocs-1
        if (k==rem) then
           Ncol = Ncol - 1
        endif
        call MPI_RECV(A, Ncol*Nrow, MPI_INTEGER, k, 0, MPI_COMM_WORLD, status, ierr)
        do j=1,Ncol
           print '('//trim(stringa)//'(I2))', A(:,j)
        enddo
     enddo
  else
     !$ Send local data to Rank 0
     call MPI_SEND(A, Nrow*Ncol, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, ierr)
  endif
  DEALLOCATE(a)
  call MPI_FINALIZE(ierr)

end program distribuite
