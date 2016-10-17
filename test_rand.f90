! ================================= 
PROGRAM test_rand 
  ! ================================= 
Implicit None

  REAL*8 X(5)
  INTEGER N(2)    
  INTEGER time(8) 
  INTEGER seed(2) 
  INTEGER nx,K
  INTEGER, DIMENSION(1):: OLD,NEW
  
!   WRITE(*,*) 'Case 1: A user calls for a random' 
!   WRITE(*,*) '        number before the seed is' 
!   WRITE(*,*) '        set.  Should return the' 
!   WRITE(*,*) '        static default' 
!   CALL RANDOM_NUMBER(HARVEST = X) 
!   WRITE(*,*), 'VAL1a= ', X 
!   CALL RANDOM_NUMBER(HARVEST = X) 
!   WRITE(*,*), 'VAL1b= ', X 
!   CALL RANDOM_NUMBER(HARVEST = X) 
!   WRITE(*,*), 'VAL1c= ', X 
! 
!   WRITE(*,*) 'Case 2: A user that call random seed without' 
!   WRITE(*,*) '        arguments should get a new random default' 
!   CALL RANDOM_SEED() 
!   CALL RANDOM_NUMBER(HARVEST = X) 
!   write(*,*), 'VAL2a= ', X  
!   CALL RANDOM_NUMBER(HARVEST = X) 
!   write(*,*), 'VAL2b= ', X 
!   CALL RANDOM_NUMBER(HARVEST = X) 
!   write(*,*), 'VAL2c= ', X 

  WRITE(*,*) 'CASE 3: A user that call random seed with' 
  WRITE(*,*) '        arguments ' 
  call DATE_AND_TIME(values=time)     ! Get the current time 
  seed(1) = time(4) * (360000*time(5) + 6000*time(6) + 100*time(7) + time(8)) 
  print *, 'SEED= ', seed(1)
  CALL RANDOM_SEED(PUT=seed) 
  CALL RANDOM_NUMBER(HARVEST = X) 
  write(*,*), 'VAL3a= ', X 
  CALL RANDOM_NUMBER(HARVEST = X) 
  write(*,*), 'VAL3b= ', X 



!   WRITE(*,*) 'CASE 4: A user that call random seed with' 
!   WRITE(*,*) '      weird arguments ' 
! 	nx=10
! 	SEED(1) = nx+1
! 	CALL RANDOM_SEED
! 	CALL RANDOM_SEED(SIZE=K)
! 	CALL RANDOM_SEED(PUT=NEW(1:K))
! 	CALL RANDOM_SEED(GET=OLD(1:K))
! 	!WRITE(*,*) ' Old starting value = ', OLD
! 	WRITE(*,*) ' K = ', K
! 	WRITE(*,*) ' NEW = ', NEW
! 	WRITE(*,*) ' OLD = ', OLD
!   	CALL RANDOM_NUMBER(HARVEST = X) 
!   	write(*,*), 'VAL4a= ', X  
!   	CALL RANDOM_NUMBER(HARVEST = X) 
!   	write(*,*), 'VAL4b= ', X 
!   	CALL RANDOM_NUMBER(HARVEST = X) 
!   	write(*,*), 'VAL4c= ', X 


END PROGRAM test_rand 