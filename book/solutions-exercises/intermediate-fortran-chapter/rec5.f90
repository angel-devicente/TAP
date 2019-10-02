PROGRAM Hanoi
  IMPLICIT NONE

  INTEGER :: pieces

  PRINT*, "Enter number of pieces"
  READ*, pieces

  CALL solve(pieces,1,3,2)

CONTAINS

  RECURSIVE SUBROUTINE solve(pieces,from,to,aux)
    INTEGER :: pieces,from,to,aux

    IF (pieces .EQ. 1) THEN
       PRINT*, pieces, "(",from," -> ",to,")"
    ELSE
       CALL solve(pieces-1,from,aux,to)
       PRINT*, pieces, "(",from," -> ",to,")"
       CALL solve(pieces-1,aux,to,from)
    END IF
  END SUBROUTINE solve
END PROGRAM Hanoi
