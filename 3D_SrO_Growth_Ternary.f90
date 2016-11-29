!! November 21st 2016 - This is designed to simulate the growth of Sr-rich particle on the Sr-rich surface of LSCF cathode material. 
!! The code incorporates the Cahn-Hilliard equation for ternary system with 3 phases.
!! The detail of the model is included in Megna Shah's thesis (Chapter 7).
!! Although the model handles ternary system, we only need to track the evolution of Sr concentration.
module simulation
implicit none

! Declare variables:---------------------------------------+
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND (p=6, r=37) ! Single data kind
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND (p=13)      ! Double data kind
!----------------------------------------------------------+

! Simulation Variables
! System Size
integer,parameter        :: nx=50, ny=50, nz=50
! Spatial and time-stepping sizes
! Spatial and time-stepping sizes
real(kind=dbl),parameter :: dt=.001d0,dx=1.d0,dy=1.d0,dz=1.d0
! Interfacial width controlling parameters
real(kind=dbl),parameter :: eps2=2.d0,W=1.d0,CSr_ini=0.10d0
! Chemical mobilities
! M_S is surface mobility of Sr 
! D_b is diffusion coefficient of Sr in the bulk LSCF and SrO
! Mobilities are different for different surfaces and bulk.
real(kind=dbl),parameter :: M_s=0.1d0,M_b=.01d0
!Equilibrium concentration of Sr and La in each phase
real(kind=dbl),parameter :: CSr_s=.08d0, CSr_p=0.95d0, CSr_v=0.d0
real(kind=dbl),parameter :: CLa_s=.82d0, CLa_p=0.0d0, CLa_v=0.d0

! I/O Variables
integer,parameter        :: it_st=1, it_ed=50000, it_mod=10000
character(len=100), parameter :: s = "SrO_on_LSCF"
character(len=10), parameter :: dates="161122_A"

end module simulation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Main Program
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program KKS_3D
use simulation
implicit none

real(kind=dbl), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: C_Sr,C_La, Pot

real(kind=dbl):: tolerance, max_c, min_c, max_phi, min_phi
integer:: iter,i,j,k

! 	iter=0
!   	call initial_conds(C_Sr,C_La)
! 	call write_output(C_Sr,C_La,iter)

	iter=10000
	call read_input(C_Sr,C_La,iter)
	write(*, '(F10.7)') (C_Sr(25,25,i), i=1,40)
	stop
	
	!!Apply No-Flux BC for the concentration of substrate material (LaCoFeO3)	
	call boundary_conds_3D(C_La)	
	
do iter=it_st,it_ed
	
	!!Apply No-Flux BC for the concentration of Sr.
	call boundary_conds_3D(C_Sr)	

	!!Calculate chem. pot. for the concentration parameter
	call Calculate_Potential(C_Sr,C_La,Pot)
	
	!!Apply  No-Flux BC for chemical potential for the Cahn-Hilliard eqn.
	call boundary_conds_3D(Pot)
	
	!! Iterate the Cahn-Hilliard Equation
	call Iterate_Cahn_Hilliard_Eqn(C_Sr,C_La,Pot)
		
	if (mod(iter,it_mod) .eq. 0) then	
		call write_output(C_Sr,C_La,iter)
	endif

enddo
write(*,*) "!!!!!!!!!!!! END Iteration !!!!!!!!!!!!!!"

end program
!*********************************************************************
!*********************************************************************
subroutine Calculate_Potential(C_Sr,C_La,Pot)
use simulation
implicit none
! Calculate the potential term in the Cahn-Hilliard equation.
	real(kind=dbl), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::C_Sr,C_La,dfdc,lap3D,Pot
	real(kind=dbl) :: a,b,d, W1,W2,W3
	integer:: i,j,k

	a=1/CSr_p
	b=-1.d0*CSr_s/(CSr_p*CLa_s)
	d=1/CLa_s
	
	W1=W
	W2=W
	W3=W
	
	dfdc=2.0d0*W1*a*(a*C_Sr+b*C_La)*(d*C_La)**2.d0 + 2.0d0*W2*a*(a*C_Sr+b*C_La)*(a*C_Sr+b*C_La-1.d0)**2.d0 + &
		 2.0d0*W2*a*(a*C_Sr+b*C_La)**2.d0*(a*C_Sr+b*C_La-1.d0)
	
! 	write(*,*) "Maximum Value of dfdc in Substrate=", MAXVAL(dfdc(:,:,1:10)) 
! 	write(*,*) "Minimum Value of dfdc in Substrate=", MINVAL(dfdc(:,:,1:10))
! 	write(*,*) "Maximum Value of dfdc in Vapor=", MAXVAL(dfdc(:,:,11:nz)) 
! 	write(*,*) "Minimum Value of dfdc in Vapor=", MINVAL(dfdc(:,:,11:nz))	
		 	
	! While the order parameter is defined in 3D, the oxide only grows on the surface and therefore the Laplacian is evaluated in 2D (surface Laplacian)
	forall(i=1:nx,j=1:ny,k=1:nz)
 		lap3D(i,j,k)=(C_Sr(i+1,j,k)+C_Sr(i-1,j,k)-2.d0*C_Sr(i,j,k))/(dx*dx) + &
 					 (C_Sr(i,j+1,k)+C_Sr(i,j-1,k)-2.d0*C_Sr(i,j,k))/(dy*dy) + &
  			     	 (C_Sr(i,j,k+1)+C_Sr(i,j,k-1)-2.d0*C_Sr(i,j,k))/(dz*dz)
   	end forall

! 	write(*,*) "Maximum Value of lap3D in Substrate=", MAXVAL(lap3D(1:nx,1:ny,1:10)) 
! 	write(*,*) "Minimum Value of lap3D in Substrate=", MINVAL(lap3D(1:nx,1:ny,1:10))

! 	write(*,*) "Maximum Value of C_Sr in Bottom Substrate=", MAXVAL(C_Sr(:,:,1:10)) 
! 	write(*,*) "Minimum Value of C_Sr in Bottom Substrate=", MINVAL(C_Sr(:,:,1:10))
! 
! 	write(*,*) "Maximum Value of lap3D in Bottom Substrate=", MAXVAL(lap3D(1:nx-1,1:ny,1:10)) 
! 	write(*,*) "Minimum Value of lap3D in Bottom Substrate=", MINVAL(lap3D(1:nx-1,1:ny,1:10))

! 	write(*,*) "Maximum Value of lap3D in Top Substrate=", MAXVAL(lap3D(1:nx,1:ny,10)) 
! 	write(*,*) "Minimum Value of lap3D in Top Substrate=", MINVAL(lap3D(1:nx,1:ny,10))
! 
! 	write(*,*) "Maximum Value of lap3D in Vapor=", MAXVAL(lap3D(1:nx,1:ny,11:nz)) 
! 	write(*,*) "Minimum Value of lap3D in Vapor=", MINVAL(lap3D(1:nx,1:ny,11:nz))	
	
!	call write(lap3D)
	
	

	Pot=dfdc-eps2*lap3D

!  	write(*,*) "Maximum Value of Pot in Top Substrate=", MAXVAL(Pot(1:nx,1:ny,10)) 
!  	write(*,*) "Minimum Value of Pot in Top Substrate=", MINVAL(Pot(1:nx,1:ny,10))
!  
!  	write(*,*) "Maximum Value of Pot in Vapor=", MAXVAL(Pot(1:nx,1:ny,12)) 
!  	write(*,*) "Minimum Value of Pot in Vapor=", MINVAL(Pot(1:nx,1:ny,12))	
! 	stop
		
end subroutine
!*********************************************************************
!*********************************************************************
subroutine Iterate_Cahn_Hilliard_Eqn(C_Sr,C_La,Pot)
use simulation
implicit none
! Iterate the Cahn-Hilliard equation.
	real(kind=dbl), DIMENSION(0:nx+1,0:ny+1,0:nz+1) ::C_Sr,C_La,Pot, Mob, g, h, Div
	real(kind=dbl) :: Ag, Ah
	integer:: i,j,k

	!Mobility control parameter such that the surface mobility and bulk mobility are different for different 
	! interfaces between phases and phase, respectively.
	Ag=1.d0
	Ag=1.d0
	g=(C_Sr-CSr_v)**2.d0*(C_Sr-CSr_p)**2.d0*(C_La-CLa_s)**2.d0+Ag*(C_La-CLa_v)**2.d0*(C_La-CLa_s)**2.d0
	h=(C_Sr-CSr_v)**2.d0+Ah*(C_La-CLa_v)**2.d0
	Mob=g+h*M_b/M_s
	Mob=1.d0
	!!Divergence of M(phi) * grad(mu)
    forall(i=1:nx,j=1:ny,k=1:nz)
    Div(i,j,k)=((Mob(i,j,k)+Mob(i+1,j,k))*(Pot(i+1,j,k)-Pot(i,j,k))-(Mob(i,j,k)+Mob(i-1,j,k))*(Pot(i,j,k)-Pot(i-1,j,k))) / (2.d0*dx*dx) + &
    		   ((Mob(i,j,k)+Mob(i,j+1,k))*(Pot(i,j+1,k)-Pot(i,j,k))-(Mob(i,j,k)+Mob(i,j-1,k))*(Pot(i,j,k)-Pot(i,j-1,k))) / (2.d0*dy*dy) + &
    		   ((Mob(i,j,k)+Mob(i,j,k+1))*(Pot(i,j,k+1)-Pot(i,j,k))-(Mob(i,j,k)+Mob(i,j,k-1))*(Pot(i,j,k)-Pot(i,j,k-1))) / (2.d0*dz*dz)
    end forall	
	
	C_Sr=C_Sr+Div*dt
	
end subroutine
!*********************************************************************
!*********************************************************************
subroutine boundary_conds_3D(phi)
use simulation
implicit none
! Impose no-flux Neumann boundary conditions
	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: phi

	!!no-flux BC 
	phi(0,:,:)=phi(1,:,:)
	phi(nx+1,:,:)=phi(nx,:,:)
	phi(:,0,:)=phi(:,1,:)
	phi(:,ny+1,:)=phi(:,ny,:)
	phi(:,:,0)=phi(:,:,1)
	phi(:,:,nz+1)=phi(:,:,nz)
		
end subroutine
!*********************************************************************
!*********************************************************************
subroutine initial_conds(C_Sr,C_La)
use simulation
implicit none

	real(kind=DBL), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: C_Sr, C_La	
	real(kind=DBL) :: circle, x_coord, y_coord, z_coord, radius, delta, xcenter, ycenter, zcenter
	integer :: i,j,k
	
!Concentration in the substrate	
	C_La(:,:,1:10)=CLa_s
	C_Sr(:,:,1:10)=CSr_ini

!Concentration in the vapor	
	C_La(:,:,21:nz)=CLa_v
	C_Sr(:,:,21:nz)=CSr_v

!Concentration in the particle
	C_La(:,:,11:20)=CLa_p
	C_Sr(:,:,11:20)=CSr_p


! The particle sits on the substrate and has a hemispherical shape.
! 	xcenter=real(nx/2,kind=dbl)
! 	ycenter=real(ny/2,kind=dbl)
! 	zcenter=real(nz/2,kind=dbl)
! 	radius=5.d0
! 	delta=1.d0
! 	do k=11,nz
! 	z_coord=real(k,kind=dbl)
! 	do i=1,nx
! 		x_coord=real(i,kind=dbl)
! 		do j=1,ny
! 			y_coord=real(j,kind=dbl)
! 			circle=sqrt((x_coord-xcenter)**2+(y_coord-ycenter)**2+(z_coord-zcenter)**2)
!     		C_Sr(i,j,k)=0.5d0*(1.d0+tanh((radius-circle)/delta))*CSr_p
! 		enddo
! 	enddo	
! 	enddo
	
!Concentration of vacancy is 1-C_La-C_Sr
		 	
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write_output(C_Sr,C_La,iter)
use simulation
implicit none
	real(kind=dbl), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: C_Sr,C_La
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Total Sr =",sum(C_Sr(1:nx,1:ny,1:nz)) 
	write(*,*) "Total Sr in Substrate =",sum(C_Sr(1:nx,1:ny,1:10)) 
	write(*,*) "Total Sr in Vapor =",sum(C_Sr(1:nx,1:ny,11:nz)) 			
	write(*,*) "Maximum Value of Conc=", MAXVAL(C_Sr) 
	write(*,*) "Minimum Value of Conc=", MINVAL(C_Sr) 	

	
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

	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_C_Sr_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) C_Sr(1:nx,1:ny,1:nz)
	close(1)	
! 
! 	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_C_La_t'//trim(iteration)//'_'//trim(dates)//'.dat'
! 	write(*,*) filename
! 	open(2,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
! 	write(2) C_La(1:nx,1:ny,1:nz)
! 	close(2)
			
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine write(phi)
use simulation
implicit none
	real(kind=dbl), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: phi
	INTEGER :: iter
	CHARACTER(LEN=100) :: filename
	CHARACTER(LEN=10) :: iteration
	CHARACTER(LEN=4) :: format_string	
	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_lap3D_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='REPLACE',ACTION='READWRITE')
	write(1) phi(1:nx,1:ny,1:nz)
	close(1)	

			
end subroutine
!*********************************************************************
!********************************************************************* 	
subroutine read_input(C_Sr,C_La,iter)
use simulation
implicit none
	real(kind=dbl), DIMENSION(0:nx+1,0:ny+1,0:nz+1) :: C_Sr,C_La
	INTEGER :: iter
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

	
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_C_Sr_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(1,file=filename,form='unformatted',STATUS='old')
	read(1) C_Sr(1:nx,1:ny,1:nz)
	close(1)	
! 
	filename='data/'//trim(s)//'/'//trim(dates)//'/'//trim(s)//'_C_La_t'//trim(iteration)//'_'//trim(dates)//'.dat'
	write(*,*) filename
	open(2,file=filename,form='unformatted',STATUS='old')
	read(2) C_La(1:nx,1:ny,1:nz)
	close(2)

	write(*,*) "Write Output at iter=",iter 
	write(*,*) "Total Sr Conc. =",sum(C_Sr(1:nx,1:ny,1:nz)) 	
	write(*,*) "Maximum Value of Conc=", MAXVAL(C_Sr) 
	write(*,*) "Minimum Value of Conc=", MINVAL(C_Sr) 	
			
end subroutine
!*********************************************************************