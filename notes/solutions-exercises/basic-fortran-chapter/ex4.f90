PROGRAM PALINDROMIC
  IMPLICIT NONE
  
  INTEGER :: num, temp, digits, i
  INTEGER, DIMENSION(:), ALLOCATABLE :: data
  LOGICAL :: palin

  PRINT*, "Enter num"
  READ*, num
  temp = num
  digits = 0

  DO WHILE (temp .GT. 0)
     temp = temp / 10
     digits = digits + 1
  END DO

  ALLOCATE(data(digits))

  temp = num
  digits=0
  DO WHILE (temp .GT. 0)
     digits = digits + 1
     data(digits) = MOD(temp,10)
     temp = temp / 10
  END DO

  palin = .TRUE.

  DO i=1,digits/2
     IF (data(i) .NE. data(digits-i+1)) THEN
        palin = .FALSE.
        EXIT
     END IF
  END DO

  IF (palin) THEN
     PRINT*, "Palindromic"
  ELSE
     PRINT*, "NOT Palindromic"
  END IF

END PROGRAM PALINDROMIC
