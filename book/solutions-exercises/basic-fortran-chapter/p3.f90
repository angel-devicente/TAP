PROGRAM EULER
  IMPLICIT NONE

  INTEGER, PARAMETER :: int16 = selected_int_kind(24)
  INTEGER (kind=int16) :: temp,dest = 600851475143_int16

  temp = 2
  DO
     IF (dest .EQ. 1) EXIT
     IF (MOD(dest,temp) .EQ. 0) THEN
        PRINT*, temp
        dest = dest/temp
        temp = 2
     END IF
     temp = temp + 1
  END DO

END PROGRAM EULER
