PROGRAM REVERSE
  IMPLICIT NONE

  INTEGER :: num
  INTEGER, DIMENSION(:), ALLOCATABLE :: dat

  PRINT*, "Enter number of data to read:"
  READ*, num
  ALLOCATE(dat(num))
  PRINT*, "Enter (in one line)", num, "integers"
  READ*, dat

  PRINT*, "The data in reverse order are:"
  PRINT*, dat(num:1:-1)

END PROGRAM REVERSE
