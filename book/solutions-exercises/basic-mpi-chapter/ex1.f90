program hello_world
  use mpi
  implicit none
  
  integer :: my_rank, p, source, dest, tag, error, numb
  integer, dimension(MPI_STATUS_SIZE) :: status
  
  call MPI_Init ( error )
  call MPI_Comm_size ( MPI_COMM_WORLD, p, error )
  call MPI_Comm_rank ( MPI_COMM_WORLD, my_rank, error )
  
  if ( my_rank == 0 ) then
     do source = 1, p-1
        call MPI_Recv(numb,1,MPI_INTEGER,source,0,MPI_COMM_WORLD,status,error)
        print*, "Greetings from process ", numb, "!"
     end do
     
  else
     call MPI_Send(my_rank,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,error)
  end if
  
  call MPI_Finalize ( error )
  
end program hello_world
