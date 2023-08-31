PROGRAM tree
  USE MPI
  IMPLICIT NONE

 !! Variables MPI (rango, numero procesadores, etc.
 !!                y para determinar mi bloque de trabajo)
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER :: my_rank, p, error
  INTEGER, DIMENSION(MPI_STATUS_SIZE) :: status
  INTEGER :: my_n, my_start, my_end

!! Variables del problema Nbody
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  INTEGER :: i,j,k,n
  REAL :: dt, t_end, t, dt_out, t_out, rs, r2, r3
  REAL, PARAMETER :: theta = 1
  REAL, DIMENSION(:), ALLOCATABLE :: m
  REAL, DIMENSION(:,:), ALLOCATABLE :: r,v,a
  REAL, DIMENSION(3) :: rji

  TYPE RANGE
     REAL, DIMENSION(3) :: min,max
  END TYPE RANGE

  TYPE CPtr
     TYPE(CELL), POINTER :: ptr
  END TYPE CPtr

  TYPE CELL
     TYPE (RANGE) :: range
     REAL, DIMENSION(3) :: part
     INTEGER :: pos
     INTEGER :: type !! 0 = no particle; 1 = particle; 2 = conglomerado
     REAL :: mass
     REAL, DIMENSION(3) :: c_o_m
     TYPE (CPtr), DIMENSION(2,2,2) :: subcell
  END TYPE CELL

  TYPE (CELL), POINTER :: head, temp_cell

!! Inicialización de MPI
!!!!!!!!!!!!!!!!!!!!!!!!
  CALL MPI_INIT ( error )
  CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, p, error )
  CALL MPI_COMM_RANK ( MPI_COMM_WORLD, my_rank, error )


!! Inicialización de matrices
!! El master lee de fichero y hace un broadcast de
!! todas las variables a todo el resto de slaves
!!
  IF ( my_rank == 0 ) THEN

     READ*, dt
     READ*, dt_out
     READ*, t_end
     READ*, n

     CALL MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(dt,1,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(dt_out,1,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(t_end,1,MPI_REAL,0,MPI_COMM_WORLD,error)
     
     ALLOCATE(m(n))
     ALLOCATE(r(n,3))
     ALLOCATE(v(n,3))
     ALLOCATE(a(n,3))
     
     DO i = 1, n
        READ*, m(i), r(i,:),v(i,:)
     END DO

     CALL MPI_BCAST(m,n,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(r,n*3,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(v,n*3,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(a,n*3,MPI_REAL,0,MPI_COMM_WORLD,error)

  ELSE

     CALL MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(dt,1,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(dt_out,1,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(t_end,1,MPI_REAL,0,MPI_COMM_WORLD,error)

     ALLOCATE(m(n))
     ALLOCATE(r(n,3))
     ALLOCATE(v(n,3))
     ALLOCATE(a(n,3))

     CALL MPI_BCAST(m,n,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(r,n*3,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(v,n*3,MPI_REAL,0,MPI_COMM_WORLD,error)
     CALL MPI_BCAST(a,n*3,MPI_REAL,0,MPI_COMM_WORLD,error)

  END IF

!! Calculamos el bloque que me toca calcular.
!! A partir de aquí ya no hay distinción master / slave excepto para imprimir.
!! Por lo demás todos los procesadores colaboran por igual.
!! Se asume que n es divisible por p, y calculamos el comienzo de mi 
!! bloque (my_start) y el final de mi bloque (my_end)
!!
  my_n = n / p
  my_start = (my_n * my_rank) + 1
  my_end = my_start + my_n - 1


!! Inicialización head node
!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE(head)

  CALL Calculate_ranges(head)
  head%type = 0
  CALL Nullify_Pointers(head)
  

!! Creación del árbol inicial
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  DO i = 1,n
     CALL Find_Cell(head,temp_cell,r(i,:))
     CALL Place_Cell(temp_cell,r(i,:),i)
  END DO
  
  CALL Borrar_empty_leaves(head)
  CALL Calculate_masses(head)

!! Calcular aceleraciones iniciales 
!! Ésto es similar a la versión serie, pero ahora las operaciones
!!sobre matrices ahora las hacemos con un rango.
!!    Por ejemplo: a(my_start:my_end,:)
!!    Es decir, sólo me encargo de trabajar sobre mi bloque. 
!!    Los otros procesadores hacen lo propio con su bloque.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  a(my_start:my_end,:) = 0.0
  CALL Calculate_forces(head,my_start,my_end)

!! Bucle principal
!!!!!!!!!!!!!!!!!!
  t_out = 0.0
  DO t = 0.0, t_end, dt
     v(my_start:my_end,:) = v(my_start:my_end,:) + a(my_start:my_end,:) * dt/2
     r(my_start:my_end,:) = r(my_start:my_end,:) + v(my_start:my_end,:) * dt

     CALL MPI_ALLGATHER(r(my_start,1),my_n,MPI_REAL,r(1,1),my_n,MPI_REAL,MPI_COMM_WORLD,error)
     CALL MPI_ALLGATHER(r(my_start,2),my_n,MPI_REAL,r(1,2),my_n,MPI_REAL,MPI_COMM_WORLD,error)
     CALL MPI_ALLGATHER(r(my_start,3),my_n,MPI_REAL,r(1,3),my_n,MPI_REAL,MPI_COMM_WORLD,error)

     !! Las posiciones han cambiado, por lo que tenemos que borrar
     !! y reinicializar el arbol
     CALL Borrar_tree(head)
     
     CALL Calculate_ranges(head)
     head%type = 0
     CALL Nullify_Pointers(head)
     
     DO i = 1,n
        CALL Find_Cell(head,temp_cell,r(i,:))
        CALL Place_Cell(temp_cell,r(i,:),i)
     END DO
     
     CALL Borrar_empty_leaves(head)
     CALL Calculate_masses(head)

     a(my_start:my_end,:) = 0.0
     CALL Calculate_forces(head,my_start,my_end)
     v(my_start:my_end,:) = v(my_start:my_end,:) + a(my_start:my_end,:) * dt/2
 
     !! Sólo imprimimos si somos el master
     !!
     IF (my_rank == 0) THEN
        t_out = t_out + dt
        IF (t_out >= dt_out) THEN
           DO i = 1,10
              PRINT*, r(i,:)
           END DO
           PRINT*, "-----------------------------------"
           PRINT*, ""
           t_out = 0.0
        END IF
     END IF

  END DO

  CALL MPI_Finalize ( error )

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate_Ranges                       !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE Calculate_Ranges(goal)
    TYPE(CELL),POINTER :: goal

    REAL, DIMENSION(3) :: mins,maxs,medios
    REAL :: span

    mins = MINVAL(r,DIM=1)
    maxs = MAXVAL(r,DIM=1)
    span = MAXVAL(maxs - mins) * 1.1 ! Le sumo un 10% para que las particulas no caigan justo en el borde
    medios = (maxs + mins) / 2.0
    goal%range%min = medios - span/2.0
    goal%range%max = medios + span/2.0
  END SUBROUTINE Calculate_Ranges

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Find_Cell                              !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Find_Cell(root,goal,part)
    REAL, DIMENSION(3) :: part
    TYPE(CELL),POINTER :: root,goal,temp
    INTEGER :: i,j,k

    SELECT CASE (root%type)
       CASE (2)
          out: DO i = 1,2
             DO j = 1,2
                DO k = 1,2
                   IF (Belongs(part,root%subcell(i,j,k)%ptr)) THEN
                      CALL Find_Cell(root%subcell(i,j,k)%ptr,temp,part)
                      goal => temp
                      EXIT out
                   END IF
                END DO
             END DO
          END DO out
       CASE DEFAULT 
          goal => root
     END SELECT
  END SUBROUTINE Find_Cell

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Place_Cell                             !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Place_Cell(goal,part,n)
    TYPE(CELL),POINTER :: goal,temp
    REAL, DIMENSION(3) :: part
    INTEGER :: n
    
    SELECT CASE (goal%type)
       CASE (0)
          goal%type = 1
          goal%part = part
          goal%pos = n
       CASE (1)
          CALL Crear_Subcells(goal)
          CALL Find_Cell(goal,temp,part)
          CALL Place_Cell(temp,part,n)
       CASE DEFAULT
          print*,"SHOULD NOT BE HERE. ERROR!"
    END SELECT
  END SUBROUTINE Place_Cell

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Crear_Subcells                         !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE Crear_Subcells(goal)
    TYPE(CELL), POINTER :: goal
    REAL,DIMENSION(3) :: part
    INTEGER :: i,j,k,n
    INTEGER, DIMENSION(3) :: octant
   
    part = goal%part
    goal%type=2

    DO i = 1,2
       DO j = 1,2
          DO k = 1,2
             octant = (/i,j,k/)
             ALLOCATE(goal%subcell(i,j,k)%ptr)
             goal%subcell(i,j,k)%ptr%range%min = Calcular_Range (0,goal,octant)
             goal%subcell(i,j,k)%ptr%range%max = Calcular_Range (1,goal,octant)
             
             IF (Belongs(part,goal%subcell(i,j,k)%ptr)) THEN
                goal%subcell(i,j,k)%ptr%part = part
                goal%subcell(i,j,k)%ptr%type = 1
                goal%subcell(i,j,k)%ptr%pos = goal%pos
             ELSE 
                goal%subcell(i,j,k)%ptr%type = 0
             END IF
             CALL Nullify_Pointers(goal%subcell(i,j,k)%ptr)
          END DO
       END DO
    END DO
  END SUBROUTINE Crear_Subcells


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Nullify_Pointers                       !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE Nullify_Pointers(goal)
    TYPE(CELL), POINTER :: goal
    INTEGER :: i,j,k
    
    DO i = 1,2
       DO j = 1,2
          DO k = 1,2
             NULLIFY(goal%subcell(i,j,k)%ptr)
          END DO
       END DO
    END DO
  END SUBROUTINE Nullify_Pointers


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Belongs                                !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION Belongs (part,goal)
    REAL, DIMENSION(3) :: part
    TYPE(CELL), POINTER :: goal
    LOGICAL :: Belongs

    IF (part(1) >= goal%range%min(1) .AND. &
         part(1) <= goal%range%max(1) .AND. &
         part(2) >= goal%range%min(2) .AND. &
         part(2) <= goal%range%max(2) .AND. &
         part(3) >= goal%range%min(3) .AND. &
         part(3) <= goal%range%max(3)) THEN
       Belongs = .TRUE.
    ELSE
       Belongs = .FALSE.
    END IF
  END FUNCTION Belongs
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calcular_Range                         !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  FUNCTION Calcular_Range (what,goal,octant)
    INTEGER :: what,n
    TYPE(CELL), POINTER :: goal
    INTEGER, DIMENSION(3) :: octant
    REAL, DIMENSION(3) :: Calcular_Range, valor_medio

    valor_medio = (goal%range%min + goal%range%max) / 2.0

    SELECT CASE (what)
    CASE (0)
       WHERE (octant == 1)
          Calcular_Range = goal%range%min
       ELSEWHERE
          Calcular_Range = valor_medio
       ENDWHERE
    CASE (1)
       WHERE (octant == 1)
          Calcular_Range = valor_medio
       ELSEWHERE
          Calcular_Range = goal%range%max
       ENDWHERE
    END SELECT
  END FUNCTION Calcular_Range

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Borrar_empty_leaves                    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Borrar_empty_leaves(goal)
    TYPE(CELL),POINTER :: goal
    INTEGER :: i,j,k
    
    IF (ASSOCIATED(goal%subcell(1,1,1)%ptr)) THEN
       DO i = 1,2
          DO j = 1,2
             DO k = 1,2
                CALL Borrar_empty_leaves(goal%subcell(i,j,k)%ptr)
                IF (goal%subcell(i,j,k)%ptr%type == 0) THEN
                   DEALLOCATE (goal%subcell(i,j,k)%ptr)
                END IF
             END DO
          END DO
       END DO
    END IF
  END SUBROUTINE Borrar_empty_leaves

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Borrar_tree                            !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Borrar_tree(goal)
    TYPE(CELL),POINTER :: goal
    INTEGER :: i,j,k
    
       DO i = 1,2
          DO j = 1,2
             DO k = 1,2
                IF (ASSOCIATED(goal%subcell(i,j,k)%ptr)) THEN
                   CALL Borrar_tree(goal%subcell(i,j,k)%ptr)
                   DEALLOCATE (goal%subcell(i,j,k)%ptr)
                END IF
             END DO
          END DO
       END DO
  END SUBROUTINE Borrar_tree

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate_masses                       !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Calculate_masses(goal)
    TYPE(CELL),POINTER :: goal
    INTEGER :: i,j,k
    REAL :: mass
    REAL, DIMENSION(3) :: c_o_m
    
    goal%mass = 0
    goal%c_o_m = 0

    SELECT CASE (goal%type)
       CASE (1)
          goal%mass = m(goal%pos)
          goal%c_o_m = r(goal%pos,:)
       CASE (2)
          DO i = 1,2
             DO j = 1,2
                DO k = 1,2
                   IF (ASSOCIATED(goal%subcell(i,j,k)%ptr)) THEN
                      CALL Calculate_masses(goal%subcell(i,j,k)%ptr)
                      mass = goal%mass
                      goal%mass = goal%mass + goal%subcell(i,j,k)%ptr%mass
                      goal%c_o_m = (mass * goal%c_o_m + goal%subcell(i,j,k)%ptr%mass * goal%subcell(i,j,k)%ptr%c_o_m) /  goal%mass
                   END IF
                END DO
             END DO
          END DO
    END SELECT
  END SUBROUTINE Calculate_masses


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate_forces                       !!
!!                                        !!
!! Muy parecida a la version en serie,    !!
!! simplemente le paso el comienzo y el   !!
!! final de mi bloque y solo llamare a la !!
!! funcion aux en caso de que la particula!!
!! a tratar sea una de ellas              !! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Calculate_forces(head,start,end)
    TYPE(CELL),POINTER :: head
    INTEGER :: i,j,k,start,end

    DO i= start,end
       CALL Calculate_forces_aux(i,head)
    END DO
    
  END SUBROUTINE Calculate_forces


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Calculate_forces_aux                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RECURSIVE SUBROUTINE Calculate_forces_aux(goal,tree)
    TYPE(CELL),POINTER :: tree
    INTEGER :: i,j,k,goal
    REAL :: l,D

    SELECT CASE (tree%type)
       CASE (1)
          IF (goal .NE. tree%pos) THEN
             rji = tree%c_o_m - r(goal,:)
             r2 = SUM(rji**2)
             r3 = r2 * SQRT(r2)
             a(goal,:) = a(goal,:) + m(tree%pos) * rji / r3
          END IF
       CASE (2)
          l = tree%range%max(1) - tree%range%min(1) !! El rango tiene el mismo span en las 3 dimensiones
          rji = tree%c_o_m - r(goal,:)
          r2 = SUM(rji**2)
          D = SQRT(r2)
          IF (l/D < theta) THEN
             !! Si conglomerado, tenemos que ver si se cumple l/D < @
             r3 = r2 * D
             a(goal,:) = a(goal,:) + tree%mass * rji / r3
          ELSE 
             DO i = 1,2
                DO j = 1,2
                   DO k = 1,2
                      IF (ASSOCIATED(tree%subcell(i,j,k)%ptr)) THEN
                         CALL Calculate_forces_aux(goal,tree%subcell(i,j,k)%ptr)
                      END IF
                   END DO
                END DO
             END DO
          END IF
    END SELECT
    
  END SUBROUTINE Calculate_forces_aux

END PROGRAM tree
