PROGRAM EULER
  IMPLICIT NONE

  INTEGER, DIMENSION(1000) :: numbers
  INTEGER, PARAMETER :: int16 = selected_int_kind(16)
  INTEGER :: pos = 1,i
  INTEGER (kind=int16) :: tsol= 0, sol = 0

  DO i = 1,20
     READ(*,'(50I1)') numbers(pos:pos+49)
     pos = pos + 50
  END DO

  DO pos=1,988
     tsol = 1
     DO i=0,12
        tsol = tsol * numbers(pos+i)
     END DO
     PRINT*, pos, tsol
     IF (tsol .GT. sol) sol = tsol
  END DO

  PRINT*, sol

END PROGRAM EULER
