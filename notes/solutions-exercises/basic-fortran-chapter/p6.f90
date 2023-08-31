PROGRAM SIX
  IMPLICIT NONE

  INTEGER, DIMENSION(100) :: numbs
  INTEGER :: i

  numbs = (/ (i, i=1,100) /)

 PRINT*,  SUM(numbs)**2 -  SUM(numbs**2)

END PROGRAM SIX
