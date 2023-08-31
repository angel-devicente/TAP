PROGRAM array_alias
  IMPLICIT NONE
 
  INTEGER, TARGET, DIMENSION(1:10) :: VECTOR
  INTEGER, POINTER, DIMENSION(:)    :: ODD, EVEN

  ODD => VECTOR(1:10:2)
  EVEN => VECTOR(2:10:2)

  EVEN = 13
  ODD = 17

  PRINT*, "whole vector:", VECTOR
  PRINT*, "odd elements:", ODD
  PRINT*, "even elements:", EVEN

END PROGRAM array_alias
