PROGRAM SALARIES_COST
  IMPLICIT NONE

  INTEGER :: n,nc,i

  REAL, DIMENSION(:), ALLOCATABLE :: salaries
  INTEGER, DIMENSION(:), ALLOCATABLE :: categories
  REAL, DIMENSION(:), ALLOCATABLE :: payrise

  REAL :: curr_cost, new_cost
  
  READ*, n
  READ*, nc

  ALLOCATE(salaries(n))
  ALLOCATE(categories(n))
  ALLOCATE(payrise(nc))

  DO i=1,n
     READ*, salaries(i)
  END DO

  DO i=1,n
     READ*, categories(i)
  END DO

  DO i=1,nc
     READ*, payrise(i)
  END DO
  payrise = 1 + payrise/100  ! Convert it to a factor
  
  curr_cost = SUM(salaries)

  new_cost = 0
  DO i=1,n
     new_cost = new_cost + salaries(i) * payrise(categories(i))
  END DO

  PRINT*, "Current cost:",curr_cost," New cost:",new_cost,"Difference:", new_cost - curr_cost
END PROGRAM SALARIES_COST
