PROGRAM TP
  IMPLICIT NONE

  INTEGER :: n,i
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: distances
  INTEGER, DIMENSION(:), ALLOCATABLE :: route, route_temp
  LOGICAL, DIMENSION(:), ALLOCATABLE :: visited
  INTEGER :: dist_travelled, min_dist = HUGE(0)
  
  READ*, n
  ALLOCATE(distances(n,n))
  ALLOCATE(route(n),route_temp(n),visited(n))

  DO i=1,n
     READ*, distances(i,:)
  END DO

  visited = .FALSE.
  dist_travelled = 0
  
  CALL tsp(1)

  PRINT*, "Shortest route travelled is :", route
  PRINT*, "Distance travelled = ", min_dist
CONTAINS

  RECURSIVE SUBROUTINE tsp (level)
    INTEGER :: level, city, cur_dist
 
    IF (level < n) THEN
       DO city=1,n
          IF (.NOT. visited(city)) THEN
             visited(city) = .TRUE.
             route_temp(level) = city
             cur_dist = dist_travelled
             IF (level > 1) THEN
                dist_travelled = dist_travelled + distances(route_temp(level-1),city)
             END IF

!             PRINT*, "At level", level, " Route", route_temp(1:level)," Distance",dist_travelled
             
             CALL tsp(level+1)

             dist_travelled = cur_dist
             visited(city) = .FALSE.
          END IF
       END DO
    ELSE
       DO city=1,n
          IF (.NOT. visited(city)) THEN
             visited(city) = .TRUE.
             route_temp(level) = city
             cur_dist = dist_travelled
             dist_travelled = dist_travelled + distances(route_temp(level-1),city) + &
                  distances(city,route_temp(1))

!             PRINT*, "CIRCUIT: ", route_temp(1:level)," Distance: ",dist_travelled
             
             IF (dist_travelled < min_dist) THEN
                min_dist = dist_travelled
                route = route_temp
!                PRINT*, "--- IMPROVEMENT: ",dist_travelled
             END IF
             
             dist_travelled = cur_dist
             visited(city) = .FALSE.
          END IF
       END DO

       
    END IF
    
  END SUBROUTINE tsp
  
END PROGRAM TP
