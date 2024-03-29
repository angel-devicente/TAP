!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  %%%%
!  %%%% This program file is part of the book and course
!  %%%% "Parallel Computing"
!  %%%% by Victor Eijkhout, copyright 2013-5
!  %%%%
!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Program Init

#include "mpif.h"

#pragma usage-f
  integer :: input_argument;
  character(len=20) :: argstring
#pragma end

#include "globalinit.F90"

#pragma usage-f
  if (mytid==0) then
    if ( command_argument_count()==0 ) then
       ! the program is called without parameter
       print *,"Usage: init [0-9]+\n"
       return
    else
       call get_command_argument(1,argstring) ! test for "-h"
       print *,argstring
       select case(adjustl(argstring))
       case("-h","--help")
          print *,"Usage: init [0-9]+\n"
          return
       case default
          ! parse input argument
          read(argstring,'(i5)') input_argument
       end select
    end if
  end if
  call MPI_Bcast(input_argument,1,MPI_INTEGER,0,comm,err)
#pragma end
  print *,"Processor",mytid,"reports",input_argument
  call MPI_Finalize(err)
  return

end program
