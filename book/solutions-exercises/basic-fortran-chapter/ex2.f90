PROGRAM leapyear
  IMPLICIT NONE

  INTEGER :: i, start, end, base
  PRINT*, "Enter starting and end years"
  READ*, start,end
  PRINT*, "Leap years between years", start, "and", end, "are:"

  base = start / 4
  IF (MOD(start,4) .NE. 0) start = (base+1)*4

  DO i=start,end,4
     IF ((MOD(i,100) .NE. 0) .OR. (MOD(i,400) .EQ. 0)) PRINT*, i
  END DO

END PROGRAM leapyear
