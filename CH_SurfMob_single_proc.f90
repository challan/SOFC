module simulation

! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

! Simulation Variables
integer,parameter        :: nx=256,ny=256,nz=256
real(kind=8),parameter :: dt=.05d0,dx=1.d0,dy=1.d0,dz=1.d0
real(kind=8),parameter :: epsilon2=0.2d0,L_c=1.0d0,W=0.4d0

! I/O Variables
character(len=100), parameter :: s = "CH_SurfMob"
integer,parameter        :: it_st=1, it_ed=200000, it_mod=20000

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program CH_SurfMob
use simulation
implicit none

integer:: iter,i,j,k

real(kind=8)::c(0:nx+1,0:ny+1,0:nz+1)
real(kind=8)::pot(0:nx+1,0:ny+1,0:nz+1)
character(len=100) :: filename

	!!Initial Conditions
	call initial_conds(c)
	
	!!Read Input
!	call read_input(c,it_st-1)


write(*,*) "!!!!!!!!!!!! Begin Iteration !!!!!!!!!!!!!!"
do iter=it_st,it_ed

	!!Apply Periodic BC for the concentration
	call boundary_conds(c)
	
	call calculate_potential(c,pot)	
	
	!!Apply Periodic BC for chemical potential
	call boundary_conds(pot)

	!!Regular Cahn-Hilliard Iteration
	!call bulk_cahn_hilliard_eqn(c,pot)

	!!Regular Cahn-Hilliard Iteration
	call surface_cahn_hilliard_eqn(c,pot)


	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(c,iter)
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"

end program
!*********************************************************************
!*********************************************************************
subroutine surface_cahn_hilliard_eqn(c,pot)
use simulation

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: c,pot,mob
	real(kind=DBL), DIMENSION(1:nx,1:ny,1:nz) :: div_surfMob_gradpot
	integer:: i,j,k
	
	div_surfMob_gradpot(:,:,:)=0.d0
	mob=c**2.d0*(1-c)**2.d0

	!!Divergence of M(phi) * grad(phi)
    forall(i=1:nx,j=1:ny,k=1:nz)
    	div_surfMob_gradpot(i,j,k)=((mob(i,j,k)+mob(i+1,j,k))*(pot(i+1,j,k)-pot(i,j,k))-(mob(i,j,k)+mob(i-1,j,k))*(pot(i,j,k)-pot(i-1,j,k))) / (2*dx*dx) + &
    							   ((mob(i,j,k)+mob(i,j+1,k))*(pot(i,j+1,k)-pot(i,j,k))-(mob(i,j,k)+mob(i,j-1,k))*(pot(i,j,k)-pot(i,j-1,k))) / (2*dy*dy) + &
    							   ((mob(i,j,k)+mob(i,j,k+1))*(pot(i,j,k+1)-pot(i,j,k))-(mob(i,j,k)+mob(i,j,k-1))*(pot(i,j,k)-pot(i,j,k-1))) / (2*dz*dz)
    end forall
 
  	!! Surface Diffusion Cahn-Hilliard Iteration
    c(1:nx,1:ny,1:nz)=c(1:nx,1:ny,1:nz)+dt*div_surfMob_gradpot(1:nx,1:ny,1:nz)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine bulk_cahn_hilliard_eqn(c,pot)
use simulation

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: c,pot
	real(kind=DBL), DIMENSION(1:nx,1:ny,1:nz) :: lap_pot
	integer:: i,j,k
	
	lap_pot(:,:,:)=0.0d0

	!!lap_pot is the laplacian of chemical potential.
    forall(i=1:nx,j=1:ny,k=1:nz)
    	lap_pot(i,j,k)=(pot(i+1,j,k)+pot(i-1,j,k)-2.d0*pot(i,j,k))/(dx*dx) &
				+(pot(i,j+1,k)+pot(i,j-1,k)-2.d0*pot(i,j,k))/(dy*dy) &
                +(pot(i,j,k+1)+pot(i,j,k-1)-2.d0*pot(i,j,k))/(dz*dz)
    end forall
 
  	!! Regular Cahn-Hilliard Iteration
    c(1:nx,1:ny,1:nz)=c(1:nx,1:ny,1:nz)+L_c*dt*lap_pot(1:nx,1:ny,1:nz)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine calculate_potential(c,pot)
use simulation

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: c,pot
	real(kind=DBL), DIMENSION(1:nx,1:ny,1:nz) :: df_dc,grad2c
	integer:: i,j,k

	df_dc(:,:,:)=0.0d0
	grad2c(:,:,:)=0.0d0

	forall(i=1:nx,j=1:ny,k=1:nz)
		df_dc(i,j,k)=.5d0*W*c(i,j,k)*(1.d0-c(i,j,k))*(1.d0-2.d0*c(i,j,k))
  
		grad2c(i,j,k)=(c(i+1,j,k)+c(i-1,j,k)-2.d0*c(i,j,k))/(dx*dx) &
              	+(c(i,j+1,k)+c(i,j-1,k)-2.d0*c(i,j,k))/(dy*dy) &
              	+(c(i,j,k+1)+c(i,j,k-1)-2.d0*c(i,j,k))/(dz*dz)  
   	end forall
 
 	! chemical potential term = pot
 	pot(1:nx,1:ny,1:nz)=df_dc(1:nx,1:ny,1:nz)-epsilon2*grad2c(1:nx,1:ny,1:nz)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds(phi)
use simulation

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: phi

	!!Periodic BC 
	phi(0,:,:)=phi(nx,:,:)
	phi(nx+1,:,:)=phi(1,:,:)
	phi(:,0,:)=phi(:,ny,:)
	phi(:,ny+1,:)=phi(:,1,:)	
	phi(:,:,0)=phi(:,:,nz)
	phi(:,:,nz+1)=phi(:,:,1)
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine initial_conds(c)
use simulation

	real(kind=8), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::c	
  	INTEGER:: seed(2), time(8), xcenter(100),ycenter(100) !Actual coordinates of the centers of the precipitates	
  	INTEGER:: i,j,k
  	CHARACTER(LEN=100) :: filename
  	
  	call DATE_AND_TIME(values=time)     ! Get the current time 
  	seed(1) = time(4) * (360000*time(5) + 6000*time(6) + 100*time(7) + time(8)) 
  	print *, 'SEED= ', seed(1)
  	CALL RANDOM_SEED(PUT=seed) 
  	CALL RANDOM_NUMBER(HARVEST = c) 

	c=0.5d0+(2.d0*c-1.0)*0.1d0	
	
	write(*,*)'Initial Condition'
	vol_fraction=sum(c(1:nx,1:ny,1:nz))	/ (real(nx*ny*nz))
	write(*,*)'Volume Fraction = ',vol_fraction			
	
	filename='data/CH_SurfMob/'//trim(s)//'_t0_161017.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) c(1:nx,1:ny,1:nz)
	close(2)

end subroutine
!*********************************************************************
!*********************************************************************
subroutine read_input(c,iter)
use simulation

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: c
	real(kind=8) ::max_c,min_c
	INTEGER :: iter,i
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

	filename='data/CH_SurfMob/'//trim(s)//'_t'//trim(iteration)//'_161017.dat'
 	write(*,*) filename
 	
 	open  ( unit = 1, file = filename, status='old', &
               form = 'unformatted')	
	read  (1) c(1:nx,1:ny,1:nz)
	close(1)
	
	max_c = MAXVAL(c) 
	min_c = MINVAL(c)
	write(*,*) "Maximum Value of C=",max_c
	write(*,*) "Minimum Value of C=",min_c
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine write_output(c,iter)
use simulation

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: c
	real(kind=DBL) :: vol_fraction
	INTEGER :: iter,i
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

	write(*,*)'Iteration = ',iter
	vol_fraction=sum(c(1:nx,1:ny,1:nz))	/ (real(nx*ny*nz))
	write(*,*)'Volume Fraction = ',vol_fraction			
	
	filename='data/CH_SurfMob/'//trim(s)//'_t'//trim(iteration)//'_161017.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(2) c(1:nx,1:ny,1:nz)
	close(2)	

end subroutine
!*********************************************************************
!*********************************************************************