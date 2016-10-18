!! Original Author Hsun-Yi Chen. This code was designed to run SBM using 3-direction MPI
!! Edited by Chal Park. This code now runs AC dynamics using 3-direction MPI.
!! Problem of this code is that number of processors per direction comes in power of two's.
!! i.e. I can only do MPI by 2x2x2, 4x4x4, 8x8x8, 16x16x16.
!! On Kraken, where each node carries 12 CPUs, this leads to waste of CPU's as multiples of 12
!! does not add up to powers of two. 
!!
!! August 9th 2012 - This code runs CH dynamics using 3-direction MPI. The CH equation comes from 
!! Yongwoo's para_CHeq.f90
!!
!! October 16th 2015 - This code combines CH_mpi3.f90 and CH_SurfMob_single_proc.f90. 
!!
!! November 7th 2015 - Finally fed up with the shortcoming of this code where it can only read in
!!					   number of cores that is powers of two. The code can now partition in
!!					   any number of cores as long as the dimension of the data array is divisible by the number of cores

module constants
implicit none
! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

INCLUDE 'mpif.h'
INTEGER, PARAMETER :: d1 = 4, d2 = 4, d3 = 4       ! partition number in each direction 
INTEGER, PARAMETER :: nx = 256, ny = 256, nz = 256 ! number of non-ghost grid in each direction
INTEGER, PARAMETER :: bb = 1                       ! layers of ghost grid
INTEGER, PARAMETER :: c1Tag=91,c2Tag=92,c3Tag=93,c4Tag=94, pTag=95
INTEGER, PARAMETER :: r1Tag=101,l1Tag=102,f1Tag=103,b1Tag=104,t1Tag=105,d1Tag=106    ! communicate tag
INTEGER, PARAMETER :: r2Tag=111,l2Tag=112,f2Tag=113,b2Tag=114,t2Tag=115,d2Tag=116    ! communicate tag

real(kind=DBL),parameter :: dt=.05d0,dx=1.d0,dy=1.d0,dz=1.d0
real(kind=DBL),parameter :: epsilon2=0.2d0,L_phi=1.d0,W=0.4d0

INTEGER(KIND=8), PARAMETER :: start_it=1,itMax=10000000,output_per_iter=1000000

end module constants
!*********************************************************************
!*********************************************************************
!*********************************************************************
!*********************************************************************
PROGRAM CH_SurfMob_mpi3
use constants
implicit none

INTEGER errcode
INTEGER rank, nsize, i, j, k, l, d, q, r, e, f
INTEGER L1, L2, L3, S1, S2, S3, Id1, Id2, Id3, Rm1, Rm2, Rm3
INTEGER nstatus(MPI_STATUS_SIZE)

call MPI_INIT (errcode)

call MPI_COMM_RANK (MPI_COMM_WORLD, rank, errcode)
call MPI_COMM_SIZE (MPI_COMM_WORLD, nsize, errcode)
!write(*,*)'rank=',rank

L1 = floor(nx/real(d1))                           ! non-ghost length of each partitioin
Rm1 = mod(nx,d1)                                  ! remanet grids, will feed to the last partition 
L2 = floor(ny/real(d2))
Rm2 = mod(ny,d2)
L3 = floor(nz/real(d3))
Rm3 = mod(nz,d3)
Id3 = floor(rank/real((d1)*(d2)))                       ! index of each partition
Id2 = floor(real((rank - id3*(d1)*(d2))/real(d1)))
Id1 = rank - id3*(d1)*(d2) - id2*(d1)

if(Id1 == 0)then                                          ! starting point of domain is set to 1, end point is nx
   S1 = 1-bb
   L1=L1+2*bb
elseif(Id1 == d1-1)then   
   S1 = Id1*L1+1-bb
   L1 = L1+2*bb+Rm1
else
   S1 = Id1*L1+1-bb
   L1= L1+2*bb
endif
if(Id2 == 0)then                                          ! starting point of domain is set to 1, end point is nx
   S2 = 1-bb
   L2= L2+2*bb
elseif(Id2 == d2-1)then   
   S2 = Id2*L2+1-bb
   L2 = L2+2*bb+Rm2
else
   S2 = Id2*L2+1-bb
   L2 = L2+2*bb
endif
if(Id3 == 0)then                                          ! starting point of domain is set to 1, end point is nx
   S3 = 1-bb
   L3= L3+2*bb
elseif(Id3 == d3-1)then   
   S3 = Id3*L3+1-bb
   L3 = L3+2*bb+Rm3
else
   S3 = Id3*L3+1-bb
   L3= L3+2*bb
endif

!if ( (rank .eq. 599) .or. (rank .eq. 600) .or. (rank .eq. 601) ) then
!write(*,*)'rank=',rank,'Id1=',Id1,'S1=',S1,'L1=',L1
!write(*,*)'rank=',rank,'Id2=',Id2,'S1=',S2,'L1=',L2
!write(*,*)'rank=',rank,'Id3=',Id3,'S1=',S3,'L1=',L3
!endif

call main(rank,nsize,Id1,Id2,Id3,S1,S2,S3,L1,L2,L3)

call MPI_FINALIZE(errcode)

END PROGRAM CH_SurfMob_mpi3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE main(rank,nsize,Id1,Id2,Id3,S1,S2,S3,L1,L2,L3)
use constants
implicit none

INTEGER errcode,nstatus(MPI_STATUS_SIZE)
INTEGER L1, L2, L3, S1, S2, S3, Id1, Id2, Id3, Is1, Is2, Is3, Ie1, Ie2, Ie3, rank, nsize
REAL(KIND=DBL),DIMENSION(S1:S1+L1-1,S2:S2+L2-1,S3:S3+L3-1) :: phi,pot
INTEGER :: i, j, k, t,pp

! real grid points inside the subdomain (without halo cells) 
Is1 = S1 + bb
Ie1 = S1 + L1 - 1 - bb
Is2 = S2 + bb
Ie2 = S2 + L2 - 1 - bb
Is3 = S3 + bb
Ie3 = S3 + L3 - 1 - bb

do pp=0,nsize-1
   if (rank==pp) then
      call initial_conds(phi,Is1,Ie1,Is2,Ie2,Is3,Ie3,rank)
   end if
end do

pot(:,:,:)=0.0d0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Time evolution
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DO t = start_it,itMax


	!! Apply periodic BCs for phi
    call commuBC(rank,Id1,Id2,Id3,L1,L2,L3,S1,S2,S3,phi)

	call calculate_potential(phi,pot,Is1,Ie1,Is2,Ie2,Is3,Ie3)
	
	!! Apply periodic BCs for the chemical potential term
	call commuBC(rank,Id1,Id2,Id3,L1,L2,L3,S1,S2,S3,pot)

	call surface_cahn_hilliard_eqn(phi,pot,Is1,Ie1,Is2,Ie2,Is3,Ie3)

	if (mod(t,output_per_iter) .eq. 0) THEN
			
		do pp=0,nsize-1
  			if (rank == pp) then
				call write_out(phi,Is1,Ie1,Is2,Ie2,Is3,Ie3,rank,t)
         	end if
		end do
   	end if

end do



END SUBROUTINE main
!*********************************************************************
!*********************************************************************
subroutine surface_cahn_hilliard_eqn(phi,pot,Is1,Ie1,Is2,Ie2,Is3,Ie3)
use constants
IMPLICIT NONE

	real(kind=DBL),dimension(Is1-1:Ie1+1,Is2-1:Ie2+1,Is3-1:Ie3+1) :: phi, pot, mob	
	real(kind=DBL),dimension(Is1:Ie1,Is2:Ie2,Is3:Ie3) :: div_surfMob_gradpot
	integer:: i,j,k, Is1, Is2, Is3, Ie1, Ie2, Ie3
	
	div_surfMob_gradpot(:,:,:)=0.d0
	mob=phi**2.d0*(1-phi)**2.d0

	!!Divergence of M(phi) * grad(phi)
    forall(i=Is1:Ie1,j=Is2:Ie2,k=Is3:Ie3)
    	div_surfMob_gradpot(i,j,k)=((mob(i,j,k)+mob(i+1,j,k))*(pot(i+1,j,k)-pot(i,j,k))-(mob(i,j,k)+mob(i-1,j,k))*(pot(i,j,k)-pot(i-1,j,k))) / (2.d0*dx*dx) + &
    							   ((mob(i,j,k)+mob(i,j+1,k))*(pot(i,j+1,k)-pot(i,j,k))-(mob(i,j,k)+mob(i,j-1,k))*(pot(i,j,k)-pot(i,j-1,k))) / (2.d0*dy*dy) + &
    							   ((mob(i,j,k)+mob(i,j,k+1))*(pot(i,j,k+1)-pot(i,j,k))-(mob(i,j,k)+mob(i,j,k-1))*(pot(i,j,k)-pot(i,j,k-1))) / (2.d0*dz*dz)
    end forall
 
  	!! Surface Diffusion Cahn-Hilliard Iteration
    phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)=phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)+dt*div_surfMob_gradpot(Is1:Ie1,Is2:Ie2,Is3:Ie3)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine bulk_cahn_hilliard_eqn(phi,pot,Is1,Ie1,Is2,Ie2,Is3,Ie3)
use constants
IMPLICIT NONE

	real(kind=DBL),dimension(Is1-1:Ie1+1,Is2-1:Ie2+1,Is3-1:Ie3+1) :: phi, pot	
	real(kind=DBL),dimension(Is1:Ie1,Is2:Ie2,Is3:Ie3) :: lap_pot
	integer:: i,j,k,Is1, Is2, Is3, Ie1, Ie2, Ie3
	
	lap_pot(:,:,:)=0.0d0

	!!lap_pot is the laplacian of chemical potential.
    forall(i=Is1:Ie1,j=Is2:Ie2,k=Is3:Ie3)
    	lap_pot(i,j,k)=(pot(i+1,j,k)+pot(i-1,j,k)-2.d0*pot(i,j,k))/(dx*dx) &
				+(pot(i,j+1,k)+pot(i,j-1,k)-2.d0*pot(i,j,k))/(dy*dy) &
                +(pot(i,j,k+1)+pot(i,j,k-1)-2.d0*pot(i,j,k))/(dz*dz)
    end forall
 
  	!! Regular Cahn-Hilliard Iteration
    phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)=phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)+L_phi*dt*lap_pot(Is1:Ie1,Is2:Ie2,Is3:Ie3)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potential(phi,pot,Is1,Ie1,Is2,Ie2,Is3,Ie3)
use constants
IMPLICIT NONE

	real(kind=DBL),dimension(Is1-1:Ie1+1,Is2-1:Ie2+1,Is3-1:Ie3+1) :: phi, pot	
	real(kind=DBL),dimension(Is1:Ie1,Is2:Ie2,Is3:Ie3) :: df_dc,grad2phi
	integer:: i,j,k, Is1, Is2, Is3, Ie1, Ie2, Ie3

	df_dc(:,:,:)=0.0d0
	grad2phi(:,:,:)=0.0d0

	forall(i=Is1:Ie1,j=Is2:Ie2,k=Is3:Ie3)
		df_dc(i,j,k)=.5d0*W*phi(i,j,k)*(1.d0-phi(i,j,k))*(1.d0-2.d0*phi(i,j,k))
  
		grad2phi(i,j,k)=(phi(i+1,j,k)+phi(i-1,j,k)-2.d0*phi(i,j,k))/(dx*dx) &
              	+(phi(i,j+1,k)+phi(i,j-1,k)-2.d0*phi(i,j,k))/(dy*dy) &
              	+(phi(i,j,k+1)+phi(i,j,k-1)-2.d0*phi(i,j,k))/(dz*dz)  
   	end forall
 
 	! chemical potential term = pot
 	pot(Is1:Ie1,Is2:Ie2,Is3:Ie3)=df_dc(Is1:Ie1,Is2:Ie2,Is3:Ie3)-epsilon2*grad2phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE commuBC(rank,Id1,Id2,Id3,L1,L2,L3,S1,S2,S3,phi1)
use constants
implicit none
!INCLUDE 'mpif.h'

INTEGER S1, S2, S3, Id1, Id2, Id3,L1,L2,L3
INTEGER i,j,k
INTEGER nstatus(MPI_STATUS_SIZE), errcode, rank
REAL(KIND=8) :: phi1(S1:S1+L1-1,S2:S2+L2-1,S3:S3+L3-1)

! Interior boundary condition and exterior boundary condition. all periodic
! communication in nx direction
IF(d1>0)THEN

  !  Interior boundary condition
  If(Id1 .NE. d1-1)call MPI_SEND(phi1(S1+L1-1-bb,S2:S2+L2-1,S3:S3+L3-1),bb*L2*L3,MPI_DOUBLE_PRECISION,rank+1,r1Tag,MPI_COMM_WORLD,errcode)    !Send real grid
  If(Id1 .NE. 0)call MPI_RECV(phi1(S1,S2:S2+L2-1,S3:S3+L3-1), bb*L2*L3,MPI_DOUBLE_PRECISION,rank-1,r1Tag,MPI_COMM_WORLD,nstatus,errcode)          	!receive ghost grid
  If(Id1 .NE. 0)call MPI_SEND(phi1(S1+bb,S2:S2+L2-1,S3:S3+L3-1), bb*L2*L3,MPI_DOUBLE_PRECISION,rank-1,l1Tag,MPI_COMM_WORLD,errcode)			!Send real grid
  If(Id1 .NE. d1-1)call MPI_RECV(phi1(S1+L1-1,S2:S2+L2-1,S3:S3+L3-1),bb*L2*L3,MPI_DOUBLE_PRECISION,rank+1,l1Tag,MPI_COMM_WORLD,nstatus,errcode)		!receive ghost grid
  !	 Exterior boundary condition
  If(Id1 .EQ. d1-1)call MPI_SEND(phi1(S1+L1-1-bb,S2:S2+L2-1,S3:S3+L3-1),bb*L2*L3,MPI_DOUBLE_PRECISION,rank-(d1-1),r1Tag,MPI_COMM_WORLD,errcode)
  If(Id1 .EQ. 0)call MPI_RECV(phi1(S1,S2:S2+L2-1,S3:S3+L3-1), bb*L2*L3,MPI_DOUBLE_PRECISION,rank+(d1-1),r1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id1 .EQ. 0)call MPI_SEND(phi1(S1+bb,S2:S2+L2-1,S3:S3+L3-1), bb*L2*L3,MPI_DOUBLE_PRECISION,rank+(d1-1),l1Tag,MPI_COMM_WORLD,errcode)
  If(Id1 .EQ. d1-1)call MPI_RECV(phi1(S1+L1-1,S2:S2+L2-1,S3:S3+L3-1),bb*L2*L3,MPI_DOUBLE_PRECISION,rank-(d1-1),l1Tag,MPI_COMM_WORLD,nstatus,errcode)
  
END IF
!  communication in ny direction
IF(d2>0)THEN

  !  Interior boundary condition
   If(Id2 .NE. d2-1)call MPI_SEND(phi1(S1:S1+L1-1,S2+L2-1-bb,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank+d1,f1Tag,MPI_COMM_WORLD,errcode)
   If(Id2 .NE. 0)call MPI_RECV(phi1(S1:S1+L1-1,S2,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank-d1,f1Tag,MPI_COMM_WORLD,nstatus,errcode)
   If(Id2 .NE. 0)call MPI_SEND(phi1(S1:S1+L1-1,S2+bb,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank-d1,b1Tag,MPI_COMM_WORLD,errcode)
   If(Id2 .NE. d2-1)call MPI_RECV(phi1(S1:S1+L1-1,S2+L2-1,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank+d1,b1Tag,MPI_COMM_WORLD,nstatus,errcode)
  !	 Exterior boundary condition   
  If(Id2 .EQ. d2-1)call MPI_SEND(phi1(S1:S1+L1-1,S2+L2-1-bb,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank-(d1)*(d2-1),f1Tag,MPI_COMM_WORLD,errcode)
  If(Id2 .EQ. 0)call MPI_RECV(phi1(S1:S1+L1-1,S2,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank+(d1)*(d2-1),f1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id2 .EQ. 0)call MPI_SEND(phi1(S1:S1+L1-1,S2+bb,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank+(d1)*(d2-1),b1Tag,MPI_COMM_WORLD,errcode)
  If(Id2 .EQ. d2-1)call MPI_RECV(phi1(S1:S1+L1-1,S2+L2-1,S3:S3+L3-1),L1*bb*L3,MPI_DOUBLE_PRECISION,rank-(d1)*(d2-1),b1Tag,MPI_COMM_WORLD,nstatus,errcode)

   
END IF
! communication in nz direction
IF(d3>0)THEN
  !  Interior boundary condition
  If(Id3 .NE. d3-1)call MPI_SEND(phi1(S1:S1+L1-1,S2:S2+L2-1,S3+L3-1-bb),L1*L2*bb,MPI_DOUBLE_PRECISION,rank+(d1*d2),t1Tag,MPI_COMM_WORLD,errcode)
  If(Id3 .NE. 0)call MPI_RECV(phi1(S1:S1+L1-1,S2:S2+L2-1,S3),L1*L2*bb,MPI_DOUBLE_PRECISION,rank-(d1*d2),t1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id3 .NE. 0)call MPI_SEND(phi1(S1:S1+L1-1,S2:S2+L2-1,S3+bb),L1*L2*bb,MPI_DOUBLE_PRECISION,rank-(d1*d2),d1Tag,MPI_COMM_WORLD,errcode)
  If(Id3 .NE. d3-1)call MPI_RECV(phi1(S1:S1+L1-1,S2:S2+L2-1,S3+L3-1),L1*L2*bb,MPI_DOUBLE_PRECISION,rank+(d1*d2),d1Tag,MPI_COMM_WORLD,nstatus,errcode)
  !	 Exterior boundary condition   
  If(Id3 .EQ. d3-1)call MPI_SEND(phi1(S1:S1+L1-1,S2:S2+L2-1,S3+L3-1-bb),L1*L2*bb,MPI_DOUBLE_PRECISION,rank-(d1*d2)*(d3-1),t1Tag,MPI_COMM_WORLD,errcode)
  If(Id3 .EQ. 0)call MPI_RECV(phi1(S1:S1+L1-1,S2:S2+L2-1,S3),L1*L2*bb,MPI_DOUBLE_PRECISION,rank+(d1*d2)*(d3-1),t1Tag,MPI_COMM_WORLD,nstatus,errcode)
  If(Id3 .EQ. 0)call MPI_SEND(phi1(S1:S1+L1-1,S2:S2+L2-1,S3+bb),L1*L2*bb,MPI_DOUBLE_PRECISION,rank+(d1*d2)*(d3-1),d1Tag,MPI_COMM_WORLD,errcode)
  If(Id3 .EQ. d3-1)call MPI_RECV(phi1(S1:S1+L1-1,S2:S2+L2-1,S3+L3-1),L1*L2*bb,MPI_DOUBLE_PRECISION,rank-(d1*d2)*(d3-1),d1Tag,MPI_COMM_WORLD,nstatus,errcode)

END IF

RETURN
END SUBROUTINE 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine read_inputs(phi,Is1,Ie1,Is2,Ie2,Is3,Ie3,rank,iter)
use constants
implicit none

INTEGER :: rank, iter, Is1, Is2, Is3, Ie1, Ie2, Ie3,i
real(kind=DBL),dimension(Is1-1:Ie1+1,Is2-1:Ie2+1,Is3-1:Ie3+1) :: phi
CHARACTER(LEN=2) :: data_rank !! rank no.
CHARACTER(LEN=100) :: filename
CHARACTER(LEN=10) :: iteration
CHARACTER(LEN=4) :: format_string	


	if (iter < 10) then
		format_string="(i1)"	
	elseif (iter < 100) then
		format_string="(i2)"	
	elseif (iter < 1000) then
		format_string="(i3)"	
	elseif (iter < 10000) then
		format_string="(i4)"
	elseif (iter < 100000) then
		format_string="(i5)"
	elseif (iter < 1000000) then
		format_string="(i6)"
	elseif (iter < 10000000) then
		format_string="(i7)"
	else
		format_string="(i8)"
	endif
	
	write(iteration,format_string)iter	

	
	write(data_rank,'(i2)')rank

	DO i=1,2
  		if(data_rank(i:i)=='')data_rank(i:i)='0'
	END DO	
	
	filename='data/'//trim(iteration)//'/CH_SurfMob_t'//trim(iteration)//'_rank'//trim(data_rank)//'.dat'
	open(2,file=filename,form='unformatted',STATUS='old')
	read(2) phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)
	close(2)
		
	if (rank .eq. 0) then
		write(*,*) 'Done Reading Input File ', filename
	endif

end subroutine read_inputs
!*********************************************************************
!*********************************************************************
!*********************************************************************
!*********************************************************************
subroutine initial_conds(phi,Is1,Ie1,Is2,Ie2,Is3,Ie3,rank)
use constants
IMPLICIT NONE

	real(kind=8), DIMENSION(Is1-1:Ie1+1,Is2-1:Ie2+1,Is3-1:Ie3+1) ::phi
  	INTEGER:: seed(2), time(8) !Actual coordinates of the centers of the precipitates	
    INTEGER :: rank, Is1, Is2, Is3, Ie1, Ie2, Ie3
	CHARACTER(LEN=2) :: data_rank !! rank no.
  	CHARACTER(LEN=100) :: filename
  	
  	call DATE_AND_TIME(values=time)     ! Get the current time 
  	seed(1) = (time(4) + rank)*(360000*time(5) + 6000*time(6) + 100*time(7) + time(8)) 
  	print *, 'SEED= ', seed(1)
  	CALL RANDOM_SEED(PUT=seed) 
  	CALL RANDOM_NUMBER(HARVEST = phi) 

	phi=0.5d0+(2.d0*phi-1.0)*0.1d0	
	
    if(rank .eq. 0) then
			print*, 'initial condition',minval(phi),maxval(phi)
    endif

	write(data_rank,'(i3)')rank
	DO i=1,2
  		if(data_rank(i:i)=='')data_rank(i:i)='0'
	END DO	

	filename='data/t0/CH_SurfMob_t0_rank'//trim(data_rank)//'.dat'
	
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)
	close(2)	

	if (rank .eq. 0) then
		write(*,*) 'Done Writing Output File ', filename
	endif

end subroutine
!*********************************************************************
!*********************************************************************
!*********************************************************************
!*********************************************************************
SUBROUTINE write_out(phi,Is1,Ie1,Is2,Ie2,Is3,Ie3,rank,iter)
use constants
implicit none
INTEGER :: rank, Is1, Is2, Is3, Ie1, Ie2, Ie3, iter, i
CHARACTER(LEN=100) :: filename
real(kind=DBL), dimension(Is1-1:Ie1+1,Is2-1:Ie2+1,Is3-1:Ie3+1) ::  phi
CHARACTER(LEN=2) :: data_rank !! rank no.
CHARACTER(LEN=10) :: iteration
CHARACTER(LEN=4) :: format_string	

	if (iter < 10) then
		format_string="(i1)"	
	elseif (iter < 100) then
		format_string="(i2)"	
	elseif (iter < 1000) then
		format_string="(i3)"	
	elseif (iter < 10000) then
		format_string="(i4)"
	elseif (iter < 100000) then
		format_string="(i5)"
	elseif (iter < 1000000) then
		format_string="(i6)"
	elseif (iter < 10000000) then
		format_string="(i7)"
	else
		format_string="(i8)"
	endif
	
	write(iteration,format_string)iter	
	
	write(data_rank,'(i2)')rank

	DO i=1,2
  		if(data_rank(i:i)=='')data_rank(i:i)='0'
	END DO	
	
	filename='data/t'//trim(iteration)//'/CH_SurfMob_t'//trim(iteration)//'_rank'//trim(data_rank)//'.dat'

	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) phi(Is1:Ie1,Is2:Ie2,Is3:Ie3)
	close(2)	

	if (rank .eq. 0) then
		write(*,*) 'Done Writing Output File ', filename
	endif
END SUBROUTINE write_out
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!